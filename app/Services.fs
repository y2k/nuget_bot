module Services

module Async =
    let rec retry count (timeMs : int) a =
        async {
            match! Async.Catch a with
            | Choice1Of2 x -> return x
            | Choice2Of2 e ->
                if count <= 0 then return raise e
                else
                    printfn "RETRY (count = %i): %O" count e
                    do! Async.Sleep timeMs
                    return! retry (count - 1) (2 * timeMs) a
        }

open System

type User = User of string
type Url = Url of string
type State = { items: Map<User, Url Set> }

type Msg =
    | UrlAdded of user: User * url: Url
    | UrlRemoved of user: User * url: Url

module MsgSerializer =
    [<CLIMutable>]
    type t =
        { id: int
          tag: int
          user: string
          url: string }

    let getId x = x.id

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

    let info = getId, serialize, deserialize

module MessageGenerator =
    let startMessage =
        "Commands (ver 0.1):\n/ls - list of packages\n/add <github url to (f|c)proj> - add package"

    let formatMessage user items =
        items
        |> Map.tryFind user
        |> Option.defaultValue Set.empty
        |> Set.toList
        |> List.map (fun (Url url) -> url)
        |> List.fold (fun a x -> a + "\n- " + x) ""
        |> fun x -> sprintf "Your subscriptions:\n%s" <| x.Trim '\n'

    let formatLsMessage = "Operation successful"

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

    let updateProj user repo version xml =
        let replace s (r: string) xml = Regex.Replace(xml, s, r)
        xml
        |> replace "<PackageId>.+?</PackageId>" ""
        |> replace "(<PropertyGroup>)" (sprintf "$1<PackageId>%s.%s</PackageId>" user repo)
        |> replace "<Version>.+?</Version>" ""
        |> replace "(<PropertyGroup>)" (sprintf "$1<Version>%s</Version>" version)

    let getAllUrl items =
        items
        |> Map.toList
        |> List.collect (fun (_, urls) -> urls |> Set.toList)

    let reduce state =
        function
        | UrlAdded (user, url) ->
            let urls =
                Map.tryFind user state.items
                |> Option.defaultValue Set.empty
                |> Set.add url

            { state with
                  items = Map.add user urls state.items }
        | UrlRemoved (user, url) ->
            let urls =
                Map.tryFind user state.items
                |> Option.defaultValue Set.empty
                |> Set.remove url

            { state with
                  items = Map.add user urls state.items }

    let parseUrl (Url url) =
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

    let private tryAddUrl user (_: State) url: Msg list =
        parseUrl url
        |> Option.map (fun _ -> [ UrlAdded(user, url) ])
        |> Option.defaultValue []

    let handleMsg (user, msg) (state: State): Msg list * string =
        match ParseMsg.parseMessage msg with
        | ParseMsg.Add url -> tryAddUrl user state (Url url), MessageGenerator.formatLsMessage
        | ParseMsg.Ls -> [], MessageGenerator.formatMessage user state.items
        | ParseMsg.Start -> [], MessageGenerator.startMessage
        | ParseMsg.Unknown -> [], sprintf "Unknown command: %s" msg

module DotnetBuild =
    open System.Net
    open System.IO

    let buildNugetFromGithub (version, zipUrl: Uri) url =
        async {
            let info = Domain.parseUrl url |> Option.get

            let zipPath = Path.GetTempFileName()

            let client = new WebClient()

            client.Headers.Add("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; .NET CLR 1.0.3705;)")

            client.DownloadFile(zipUrl, zipPath)

            let zipDir = Path.GetTempFileName() + "_dir"

            Compression.ZipFile.ExtractToDirectory(zipPath, zipDir)

            let slnDir = Directory.GetDirectories(zipDir).[0]

            let fp = Path.Combine(slnDir, info.proj)

            IO.File.ReadAllText(fp)
            |> Domain.updateProj info.user info.repo version
            |> fun xml -> File.WriteAllText(fp, xml)

            let projDir =
                Path.Combine(slnDir, Path.GetDirectoryName(info.proj))

            let pi =
                Diagnostics.ProcessStartInfo
                    (FileName = "dotnet", WorkingDirectory = projDir, Arguments = "pack -c Release")

            Diagnostics.Process.Start(pi).WaitForExit()

            return Path.Combine
                       (slnDir,
                        Path.GetDirectoryName(info.proj),
                        sprintf "bin/Release/%s.%s.%s.nupkg" info.user info.repo version)
        }

module BotService =
    let start (reducer: (State -> Msg list) -> State Async) listenTelegram writeTelegram =
        listenTelegram (fun msg ->
            async {
                let f1 state = fst <| Domain.handleMsg msg state
                let f2 state = snd <| Domain.handleMsg msg state
                let! state' = reducer f1
                let outMsg = f2 state'
                do! writeTelegram (fst msg) outMsg
            })

module SyncService =
    let private uploadNewVersion githubInfo pushToNuget url =
        async {
            let! ngpackPath = DotnetBuild.buildNugetFromGithub githubInfo url
            do! pushToNuget ngpackPath
        }

    let private getVersionOnNuget nugetGetLastVersion url =
        async {
            let nugetId = Domain.getNugetId url |> Option.get
            let! result = nugetGetLastVersion nugetId
            return result |> Option.map Version
        }

    let private getGithubVersion githubGetAllReleases url =
        async {
            let info = Domain.parseUrl url |> Option.get
            let! rs = githubGetAllReleases info.user info.repo
            return rs |> Seq.tryHead
        }

    let run (reducer: (State -> Msg list) -> State Async) nugetGetLastVersion githubGetAllReleases pushToNuget =
        async {
            let! state = reducer (fun _ -> [])
            let (items: Url list) = Domain.getAllUrl state.items

            for url in items do
                let! githubInfo = getGithubVersion githubGetAllReleases url
                let! currentVersion = getVersionOnNuget nugetGetLastVersion url

                match githubInfo with
                | Some ((version, _) as rs) when (Some <| Version version) <> currentVersion ->
                    do! uploadNewVersion rs pushToNuget url
                | _ -> ()
        }

    let runLoop reducer pushToNuget nugetGetLastVersion githubGetAllReleases =
        async {
            while true do
                do! run reducer nugetGetLastVersion githubGetAllReleases pushToNuget
                do! Async.Sleep 180_000
        }
