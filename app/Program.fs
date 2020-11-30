module App

open System
open Services

module Persistent =
    open LiteDB
    open System.Text.Json
    open System.Text.Json.Serialization

    let options = JsonSerializerOptions()
    options.Converters.Add(JsonFSharpConverter())

    [<CLIMutable>]
    type Wrapper = { id: int; value: string }

    type 'at t =
        { col: Wrapper ILiteCollection
          db: LiteDatabase }

    let make (db: LiteDatabase) =
        let col = db.GetCollection<Wrapper>("log")
        { col = col; db = db }

    let private serialize x = JsonSerializer.Serialize(x, options)
    let private deserialize (s: string) = JsonSerializer.Deserialize(s, options)

    let mkReducer (init: 's) (f: 's -> 'msg -> 's) (t: 'msg t) =
        let index = ref 0
        let state = ref init

        fun (fxs: _ -> 'msg list) ->
            async {
                t.db.BeginTrans() |> ignore

                let prevs =
                    t.col.Find(Query.GT("_id", BsonValue.op_Implicit !index))

                for aw in prevs do
                    let a = aw.value
                    let x = deserialize a
                    index := aw.id
                    state := f !state x

                let result = !state

                let xs = fxs !state

                for x in xs do
                    state := f !state x
                    let a = serialize x
                    let aw = { id = 0; value = a }
                    t.col.Insert aw |> ignore
                    index := aw.id

                t.db.Commit() |> ignore

                return result
            }

module Github =
    open Octokit

    let getAllReleases token (user: string) repo =
        async {
            let github =
                GitHubClient(ProductHeaderValue("nuget-bot"), Credentials = Credentials token)

            let! releases = github.Repository.Release.GetAll(user, repo)

            return
                releases
                |> Seq.map (fun rs -> rs.TagName, Uri rs.ZipballUrl)
        }

module Nuget =
    open System.Net
    open System.Text.Json

    [<CLIMutable>]
    type NugetResponseItem = { upper: string }

    [<CLIMutable>]
    type NugetResponse = { items: NugetResponseItem [] }

    let private downloadJson (client: WebClient) (url: string) =
        async {
            let! response = client.AsyncDownloadString(Uri url) |> Async.catch

            return
                match response with
                | Ok x -> Some x
                | Error e when e.Message.Contains "404" -> None
                | Error e -> raise e
        }

    let getLastVersion (id: string) =
        async {
            let client = new WebClient()
            client.Headers.Add("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; .NET CLR 1.0.3705;)")

            let url =
                sprintf "https://www.myget.org/F/y2k/api/v3/registration1/%s/index.json" (id.ToLowerInvariant())

            let! json = downloadJson client url |> Async.retry 5 2_000

            return
                json
                |> Option.map
                    (fun json ->
                        let response: NugetResponse = JsonSerializer.Deserialize(json)
                        response.items.[0].upper)
        }

    let pushToNuget token packPath =
        async {
            let pi = Diagnostics.ProcessStartInfo()
            pi.FileName <- "dotnet"
            pi.Arguments <- sprintf "nuget push -k %s -s https://www.myget.org/F/y2k/api/v2/package %s" token packPath
            let p = Diagnostics.Process.Start(pi)
            p.WaitForExit()
        }

module Telegram =
    open Telegram.Bot
    open Telegram.Bot.Types

    type t = private { client: TelegramBotClient }

    let mkClient token = { client = TelegramBotClient token }

    let private clearHistory offset (client: TelegramBotClient) =
        async {
            let stop = ref false

            while not !stop do
                let! upds = client.GetUpdatesAsync(offset = !offset, timeout = 0)

                offset
                := upds
                   |> Seq.map (fun x -> x.Id + 1)
                   |> Seq.append [ !offset ]
                   |> Seq.max

                stop := Seq.isEmpty upds
        }

    let listenUpdates t listener =
        async {
            let offset = ref 0
            do! clearHistory offset t.client

            while true do
                let! upds = t.client.GetUpdatesAsync(offset = !offset, timeout = 15 * 60)

                offset
                := upds
                   |> Seq.map (fun x -> x.Id + 1)
                   |> Seq.append [ !offset ]
                   |> Seq.max

                printfn "Updates.size = %O, offset = %O" (Seq.length upds) !offset

                for u in upds do
                    do! listener (Services.User <| string u.Message.From.Id, u.Message.Text)
        }

    let writeTelegram t (User user) message: _ Async =
        t.client.SendTextMessageAsync(ChatId.op_Implicit user, message)
        |> Async.AwaitTask
        |> Async.Ignore

[<EntryPoint>]
let main argv =
    let mkReducer db =
        Persistent.mkReducer { items = Map.empty } Domain.reduce db

    IO.Directory.CreateDirectory "__data" |> ignore

    let db: Msg Persistent.t =
        Persistent.make (new LiteDB.LiteDatabase("__data/log2.db"))

    let mygetToken = argv.[0]
    let telegramToken = argv.[1]
    let githubToken = argv.[2]
    let client = Telegram.mkClient telegramToken

    Async.Parallel [ SyncService.runLoop
                         (mkReducer db)
                         (Nuget.pushToNuget mygetToken)
                         Nuget.getLastVersion
                         (Github.getAllReleases githubToken)
                     BotService.start (mkReducer db) (Telegram.listenUpdates client) (Telegram.writeTelegram client) ]
    |> Async.RunSynchronously
    |> ignore

    0
