module App

type Url = Url of string

[<Struct>]
type Version = Version of string

module MessageGenerator =
    let formatMessage urls =
        urls
        |> Map.toSeq
        |> Seq.map (fun ((Url url), _) -> url)
        |> Seq.fold (fun a x -> a + "\n- " + x) ""
        |> fun x -> sprintf "Your subscriptions:\n%s" <| x.Trim '\n'

    let formatLsMessage _ = "Operation successful"

module ParseMsg =
    type t =
        | Start
        | Add of string
        | Ls
        | Unknown

    let parseMessage (msg: string): t =
        match msg.Split ' ' |> List.ofSeq with
        | [ "/add"; url ] -> Add url
        | [ "/ls" ] -> Ls
        | [ "/start" ] -> Start
        | _ -> Unknown

module Domain =
    open System.Text.RegularExpressions

    let parseUrl url =
        match Regex.Match(url, "github\\.com/(.+?)/(.+?)/blob/master/(.+\\.fsproj)$").Groups
              |> Seq.toList with
        | [ _; a; b; c ] ->
            Some
                {| user = a.Value
                   repo = b.Value
                   proj = c.Value |}
        | _ -> None

    let getNugetId githubUrl =
        parseUrl githubUrl
        |> Option.map (fun x -> sprintf "%s.%s" x.user x.repo)

    let private tryAddUrl db url =
        parseUrl url
        |> Option.map (fun _ -> Map.add (Url url) (Version "?") db)
        |> Option.defaultValue db

    let handleMsg state (_, msg) =
        match ParseMsg.parseMessage msg with
        | ParseMsg.Add url -> tryAddUrl state url, MessageGenerator.formatLsMessage ()
        | ParseMsg.Ls -> state, state |> MessageGenerator.formatMessage
        | ParseMsg.Start ->
            state, sprintf "Commands:\n/ls - list of packages\n/add <github url to (f|c)proj> - add package"
        | ParseMsg.Unknown -> state, sprintf "Unknown command: %s" msg

module Github =
    open Octokit

    type Release = { version: string; zipUrl: string }

    let getAllReleases (user: string) repo =
        async {
            let github =
                GitHubClient(ProductHeaderValue("nuget-bot"))

            let! releases =
                github.Repository.Release.GetAll(user, repo)
                |> Async.AwaitTask

            return releases
                   |> Seq.map (fun rs ->
                       { version = rs.TagName
                         zipUrl = rs.ZipballUrl })
        }

    let getGithubVersion githubGetAllReleases (Url url) =
        async {
            let info = Domain.parseUrl url |> Option.get
            let! (rs: #seq<Release>) = githubGetAllReleases info.user info.repo
            return rs |> Seq.tryHead
        }

module DotnetBuild =
    open System
    open System.Text.RegularExpressions
    open System.Net
    open System.IO
    open System.IO.Compression

    let buildNugetFromGithub githubGetAllReleases url =
        async {
            let info = Domain.parseUrl url |> Option.get

            let! githubInfo = Github.getGithubVersion githubGetAllReleases (Url url)

            let rs = githubInfo |> Option.get

            let zipPath = Path.GetTempFileName()

            let client = new WebClient()

            client.Headers.Add("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; .NET CLR 1.0.3705;)")

            client.DownloadFile(rs.zipUrl, zipPath)

            let zipDir = Path.GetTempFileName() + "_dir"

            ZipFile.ExtractToDirectory(zipPath, zipDir)

            let slnDir = Directory.GetDirectories(zipDir).[0]

            let fp = Path.Combine(slnDir, info.proj)

            let xml =
                IO.File.ReadAllText(fp)
                |> fun xml -> Regex.Replace(xml, "<PackageId>.+?</PackageId>", "")
                |> fun xml ->
                    Regex.Replace
                        (xml, "(<PropertyGroup>)", sprintf "$1<PackageId>%s.%s</PackageId>" info.user info.repo)
                |> fun xml -> Regex.Replace(xml, "<Version>.+?</Version>", "")
                |> fun xml -> Regex.Replace(xml, "(<PropertyGroup>)", sprintf "$1<Version>%O</Version>" rs.version)

            File.WriteAllText(fp, xml)

            let projDir =
                Path.Combine(slnDir, Path.GetDirectoryName(info.proj))

            let pi =
                Diagnostics.ProcessStartInfo
                    (FileName = "dotnet", WorkingDirectory = projDir, Arguments = "pack -c Release")

            let p = Diagnostics.Process.Start(pi)

            p.WaitForExit()

            return Path.Combine
                       (slnDir,
                        Path.GetDirectoryName(info.proj),
                        sprintf "bin/Release/%s.%s.%s.nupkg" info.user info.repo rs.version)
        }

module DotnetNuget =
    open System
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
                    do! listener (u.Message.From.Id, u.Message.Text)
        }

    let writeTelegram t (user: int) message: _ Async =
        t.client.SendTextMessageAsync(ChatId.op_Implicit user, message)
        |> Async.AwaitTask
        |> Async.Ignore

module Database =
    open System
    open LiteDB

    [<CLIMutable>]
    type 't Item = { id: string; value: 't }

    type t<'key, 't when 'key: comparison> =
        { db: LiteDatabase
          collection: ILiteCollection<'t Item>
          keyConvert: 'key -> string
          backKeyConvert: string -> 'key
          state: Map<'key, 't> ref }

    let private readAllFromDbInner backKeyConvert (collection: _ ILiteCollection) =
        collection.FindAll()
        |> Seq.map (fun x -> backKeyConvert x.id, x.value)
        |> Map.ofSeq

    let mkDatabase (collectionName: string) keyConvert backKeyConvert =
        let db = new LiteDatabase(new IO.MemoryStream())
        let col = db.GetCollection<_ Item>(collectionName)
        { db = db
          collection = col
          keyConvert = keyConvert
          backKeyConvert = backKeyConvert
          state = ref <| readAllFromDbInner backKeyConvert col }

    let saveAll t newState =
        let currentState = !t.state
        currentState
        |> Map.filter (fun k _ -> not <| Map.containsKey k newState)
        |> Map.iter (fun k _ ->
            t.collection.Delete(BsonValue.op_Implicit (t.keyConvert k))
            |> ignore)
        newState
        |> Map.filter (fun k v -> Map.tryFind k currentState <> Some v)
        |> Map.iter (fun k v ->
            t.collection.Upsert({ id = t.keyConvert k; value = v })
            |> ignore)
        t.state := newState

    let readAllFromDb t =
        readAllFromDbInner t.backKeyConvert t.collection

module BotService =
    let start listenTelegram writeTelegram db =
        listenTelegram (fun msg ->
            async {
                let state = Database.readAllFromDb db
                let (state', outMsg) = Domain.handleMsg state msg
                Database.saveAll db state'
                do! writeTelegram (fst msg) outMsg
            })

module SyncService =
    let private uploadNewVersion githubGetAllReleases pushToNuget (Url url) =
        async {
            let! ngpackPath = DotnetBuild.buildNugetFromGithub githubGetAllReleases url
            do! pushToNuget ngpackPath
        }

    let private getVersionOnNuget nugetGetLastVersion (Url url) =
        async {
            let nugetId = Domain.getNugetId url |> Option.get
            let! result = nugetGetLastVersion nugetId
            return result |> Option.map Version
        }

    let run nugetGetLastVersion githubGetAllReleases pushToNuget db =
        async {
            let (items: Map<Url, Version>) = Database.readAllFromDb db
            for (url, _) in items |> Map.toList do
                let! githubInfo = Github.getGithubVersion githubGetAllReleases url

                let version =
                    githubInfo
                    |> Option.map (fun x -> Version x.version)

                let! currentVersion = getVersionOnNuget nugetGetLastVersion url

                if Option.isSome version && version <> currentVersion
                then do! uploadNewVersion githubGetAllReleases pushToNuget url
        }

    let runLoop nugetGetLastVersion githubGetAllReleases db nugetToken =
        async {
            while true do
                do! run nugetGetLastVersion githubGetAllReleases (DotnetNuget.pushToNuget nugetToken) db
                do! Async.Sleep 60_000
        }

[<EntryPoint>]
let main argv =
    let db =
        Database.mkDatabase "main" (fun (Url x) -> x) Url

    let mygetToken = argv.[0]
    let telegramToken = argv.[1]
    let client = Telegram.mkClient telegramToken
    Async.Parallel [ SyncService.runLoop DotnetNuget.getLastVersion Github.getAllReleases db mygetToken
                     BotService.start (Telegram.listenUpdates client) (Telegram.writeTelegram client) db ]
    |> Async.RunSynchronously
    |> ignore
    0
