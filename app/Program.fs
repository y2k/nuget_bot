module App

open System
open Services
open MyGetBot

module Github =
    open Octokit

    let getAllReleases token (user : string) repo =
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
    type NugetResponseItem = { upper : string }

    [<CLIMutable>]
    type NugetResponse = { items : NugetResponseItem [] }

    let private downloadJson (client : WebClient) (url : string) =
        async {
            let! response = client.AsyncDownloadString(Uri url) |> Async.catch

            return
                match response with
                | Ok x -> Some x
                | Error e when e.Message.Contains "404" -> None
                | Error e -> raise e
        }

    let getLastVersion (id : string) =
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

    type t = { client : TelegramBotClient }

    let mkClient token = { client = TelegramBotClient token }

    let private clearHistory offset (client : TelegramBotClient) =
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

[<EntryPoint>]
let main argv =
    IO.Directory.CreateDirectory "__data" |> ignore

    let reducer =
        new LiteDB.LiteDatabase("__data/log.db")
        |> EventStore.mkReducer MsgSerializer.info State.Empty Domain.reduce

    let mygetToken, telegramToken, githubToken = argv.[0], argv.[1], argv.[2]
    let client = Telegram.mkClient telegramToken

    Async.Parallel [ SyncService.runLoop
                         reducer
                         (Nuget.pushToNuget mygetToken)
                         Nuget.getLastVersion
                         (Github.getAllReleases githubToken)
                     BotService.start reducer (Telegram.listenUpdates client) (Telegram.writeTelegram client) ]
    |> Async.RunSynchronously
    |> ignore

    0
