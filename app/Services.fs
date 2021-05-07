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
open MyGetBot

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
