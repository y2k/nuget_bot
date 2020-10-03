module App

open System
open Services

module Persistent =
    module DbAction =
        [<CLIMutable>]
        type t =
            { id: int
              tag: int
              user: string
              url: string }

        let serialize =
            function
            | UrlAdded ((User user), (Url url)) ->
                { id = 0
                  tag = 1
                  user = user
                  url = url }
            | UrlRemoved ((User user), (Url url)) ->
                { id = 0
                  tag = 2
                  user = user
                  url = url }

        let deserialize =
            function
            | { tag = 1; user = user; url = url } -> UrlAdded(User user, Url url)
            | { tag = 2; user = user; url = url } -> UrlRemoved(User user, Url url)
            | value -> failwithf "Can't parse %O" value

    open LiteDB

    type t =
        { col: DbAction.t ILiteCollection
          db: LiteDatabase }

    let make (db: LiteDatabase) =
        let col = db.GetCollection<DbAction.t>("log")
        { col = col; db = db }

    let run (init: 's) (f: 's -> Msg -> 's) (t: t) =
        let db = t.db
        let col = t.col
        let index = ref 0
        let state = ref init
        fun (fxs: _ -> Msg list) ->
            async {
                db.BeginTrans() |> ignore

                let prevs = col.Find(fun x -> x.id > !index)
                for a in prevs do
                    let x = DbAction.deserialize a
                    index := a.id
                    state := f !state x

                let result = !state

                let xs = fxs !state
                for x in xs do
                    state := f !state x
                    let a = DbAction.serialize x
                    col.Insert a |> ignore
                    index := a.id

                db.Commit() |> ignore

                return result
            }

module Github =
    open Octokit

    let getAllReleases token (user: string) repo =
        async {
            let github =
                GitHubClient(ProductHeaderValue("nuget-bot"), Credentials = Credentials token)

            let! releases =
                github.Repository.Release.GetAll(user, repo)
                |> Async.AwaitTask

            return releases
                   |> Seq.map (fun rs -> rs.TagName, Uri rs.ZipballUrl)
        }

module Nuget =
    open System.Net
    open System.Text.Json

    type NugetResponse = { items: {| upper: string |} [] }

    let getLastVersion (id: string) =
        async {
            let client = new WebClient()
            client.Headers.Add("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; .NET CLR 1.0.3705;)")

            let url =
                sprintf "https://www.myget.org/F/y2k/api/v3/registration1/%s/index.json" (id.ToLowerInvariant())

            let! response =
                client.DownloadStringTaskAsync url
                |> Async.AwaitTask
                |> Async.Catch

            let json =
                match response with
                | Choice1Of2 x -> Some x
                | Choice2Of2 e when e.Message.Contains "404" -> None
                | Choice2Of2 e -> raise e

            return json
                   |> Option.map (fun json ->
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

    type t = { client: TelegramBotClient }

    let mkClient token = { client = TelegramBotClient token }

    let private clearHistory offset (client: TelegramBotClient) =
        async {
            let stop = ref false
            while not !stop do
                let! upds =
                    client.GetUpdatesAsync(offset = !offset, timeout = 0)
                    |> Async.AwaitTask

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
                let! upds =
                    t.client.GetUpdatesAsync(offset = !offset, timeout = 60)
                    |> Async.AwaitTask

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
        Persistent.run { items = Map.empty } Domain.reduce db

    let db =
        Persistent.make (new LiteDB.LiteDatabase(new IO.MemoryStream()))

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
