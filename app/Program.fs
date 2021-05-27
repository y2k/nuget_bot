module App

open System
open Services
open MyGetBot

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
                        let response : NugetResponse = JsonSerializer.Deserialize(json)
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
                    do! listener (MyGetBot.User <| string u.Message.From.Id, u.Message.Text)
        }

    let writeTelegram t (User user) message : _ Async =
        t.client.SendTextMessageAsync(ChatId.op_Implicit user, message)
        |> Async.AwaitTask
        |> Async.Ignore

let mkReducer reduce =
    { new IReducer<State, Msg list> with
        member _.Invoke f =
            async {
                let! oldDb = reduce (fun db -> db, f db |> snd)
                return f oldDb |> fst
            } }

[<EntryPoint>]
let main argv =
    async {
        IO.Directory.CreateDirectory "__data" |> ignore
        let db = new LiteDB.LiteDatabase("__data/log.db")

        let mygetToken, telegramToken, githubToken = argv.[0], argv.[1], argv.[2]
        let tgClient = Telegram.mkClient telegramToken

        let store = EventPersistent.Store.init ()

        let commonStore =
            EventPersistent.Store.make store State.Empty Domain.reduce
            |> mkReducer

        do! EventStore.restore db MsgSerializer.deserialize (EventPersistent.Store.make store () always)

        do!
            [ EventStore.dumpEvents
                db
                MsgSerializer.serialize
                (EventPersistent.Store.make store EventStore.empty EventStore.merge)
              SyncService.runLoop
                  commonStore
                  (Nuget.pushToNuget mygetToken)
                  Nuget.getLastVersion
                  (Github.getAllReleases githubToken)
              BotService.start commonStore (Telegram.listenUpdates tgClient) (Telegram.writeTelegram tgClient) ]
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously

    0
