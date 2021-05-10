module Services

module Async =
    let rec retry count (timeMs : int) a =
        async {
            match! Async.Catch a with
            | Choice1Of2 x -> return x
            | Choice2Of2 e ->
                if count <= 0 then
                    return raise e
                else
                    printfn "RETRY (count = %i): %O" count e
                    do! Async.Sleep timeMs
                    return! retry (count - 1) (2 * timeMs) a
        }

open System
open MyGetBot

module DotnetBuild =
    open System.Net
    open System.IO

    let buildNugetFromGithub (version, zipUrl : Uri) url =
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

            File.ReadAllText(fp)
            |> Domain.updateProj info.user info.repo version
            |> fun xml -> File.WriteAllText(fp, xml)

            let projDir =
                Path.Combine(slnDir, Path.GetDirectoryName(info.proj))

            let pi =
                Diagnostics.ProcessStartInfo(
                    FileName = "dotnet",
                    WorkingDirectory = projDir,
                    Arguments = "pack -c Release"
                )

            Diagnostics.Process.Start(pi).WaitForExit()

            return
                Path.Combine(
                    slnDir,
                    Path.GetDirectoryName(info.proj),
                    sprintf "bin/Release/%s.%s.%s.nupkg" info.user info.repo version
                )
        }

module BotService =
    let start (reducer : IReducer<State, Msg list>) listenTelegram writeTelegram =
        listenTelegram
            (fun msg ->
                async {
                    let! botResponse = reducer.Invoke(Domain.handleMsg msg)
                    do! writeTelegram (fst msg) botResponse
                })

module SyncService =
    let private uploadNewVersion pushToNuget url githubInfo =
        async {
            printfn "LOG: Upload nuget package. Url: %O, GithubInfo: %O" url githubInfo

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

    let private isNeedSync state =
        if state.syncRequested then
            state.items, [ SyncRequested false ]
        else
            Map.empty, []

    let private getNewUploadTask githubInfo currentVersion =
        match githubInfo with
        | Some (version, _ as rs) when (Some <| Version version) <> currentVersion -> Some rs
        | _ -> None

    let run (reducer : IReducer<State, Msg list>) nugetGetLastVersion githubGetAllReleases pushToNuget =
        async {
            let! items = reducer.Invoke isNeedSync

            for projUrl in Domain.getAllUrl items do
                let! githubInfo = getGithubVersion githubGetAllReleases projUrl
                let! currentVersion = getVersionOnNuget nugetGetLastVersion projUrl

                match getNewUploadTask githubInfo currentVersion with
                | Some rs -> do! uploadNewVersion pushToNuget projUrl rs
                | None -> ()
        }

    let runLoop reducer pushToNuget nugetGetLastVersion githubGetAllReleases =
        async {
            while true do
                do! run reducer nugetGetLastVersion githubGetAllReleases pushToNuget
                do! Async.Sleep 15_000
        }
