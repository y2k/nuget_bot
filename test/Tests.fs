module Tests

open System
open Xunit
open App

[<Fact>]
let ``test bot commands ls`` () =
    async {
        let db =
            Database.mkDatabase "main" (fun (Url x) -> x) Url

        let mutable log = []
        let mutable writeToBot = fun _ -> failwith "writeToBot not set"

        do! BotService.start (fun f ->
                writeToBot <- f
                async.Zero()) (fun _ msg ->
                log <- msg :: log
                async.Zero()) db

        do! writeToBot (0, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")

        log <- []
        do! writeToBot (0, "/ls")
        Assert.Equal(box [ "- https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj" ], log)
    }
    |> Async.RunSynchronously

[<Fact>]
let ``test bot commands add`` () =
    async {
        let db =
            Database.mkDatabase "main" (fun (Url x) -> x) Url

        let mutable log = []
        let mutable writeToBot = fun _ -> failwith "writeToBot not set"

        do! BotService.start (fun f ->
                writeToBot <- f
                async.Zero()) (fun _ msg ->
                log <- msg :: log
                async.Zero()) db

        do! writeToBot (0, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
        Assert.Equal(box [ "Operation successful" ], log)
    }
    |> Async.RunSynchronously

module TestUtils =
    open Octokit

    let createNewRelease url =
        async {
            let! githubInfo = Github.getGithubVersion url
            let info = githubInfo |> Option.get

            let cv = System.Version(info.version)

            let v =
                System.Version(cv.Major, cv.Minor, cv.Build + 1)

            let github =
                GitHubClient
                    (ProductHeaderValue("nuget-bot"),
                     Credentials = Credentials(Environment.GetEnvironmentVariable "GITHUB_TOKEN"))

            github.Repository.Release.Create("y2k", "nuget-test", NewRelease(v.ToString())).Wait()

            return v
        }

[<Fact>]
let ``e2e test 2`` () =
    async {
        let db =
            Database.mkDatabase "main" (fun (Url x) -> x) Url

        let mutable log = []
        let mutable writeToBot = fun _ -> failwith "writeToBot not set"

        do! BotService.start (fun f ->
                writeToBot <- f
                async.Zero()) (fun _ msg ->
                log <- msg :: log
                async.Zero()) db

        do! writeToBot (0, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
        Assert.Equal(box [ "Operation successful" ], log)

        let! expected =
            // ---
            Github.getGithubVersion (Url "https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
            |> fun a -> async.Bind(a, (fun x -> x.Value.version |> System.Version |> async.Return))

        let rec whatForSync count =
            async {
                let! githubInfo =
                    Github.getGithubVersion (Url "https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")

                let actual =
                    githubInfo
                    |> Option.map (fun x -> System.Version x.version)
                    |> Option.get

                if expected <> actual then
                    if count = 0
                    then failwithf "Version not updated to %O" expected
                    do! Async.Sleep 2000
                    do! whatForSync (count - 1)
            }

        do! whatForSync 10

        do! SyncService.run (fun _ -> failwith "'pushToNuget' must not be called") db
    }
    |> Async.RunSynchronously

[<Fact>]
let ``e2e test`` () =
    async {
        let db =
            Database.mkDatabase "main" (fun (Url x) -> x) Url

        let mutable log = []
        let mutable writeToBot = fun _ -> failwith "writeToBot not set"

        do! BotService.start (fun f ->
                writeToBot <- f
                async.Zero()) (fun _ msg ->
                log <- msg :: log
                async.Zero()) db

        do! writeToBot (0, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
        Assert.Equal(box [ "Operation successful" ], log)

        let! expected =
            TestUtils.createNewRelease (Url "https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")

        let rec whatForSync count =
            async {
                let! githubInfo =
                    Github.getGithubVersion (Url "https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")

                let actual =
                    githubInfo
                    |> Option.map (fun x -> System.Version x.version)
                    |> Option.get

                if expected <> actual then
                    if count = 0
                    then failwithf "Version not updated to %O" expected
                    do! Async.Sleep 2000
                    do! whatForSync (count - 1)
            }

        do! whatForSync 10

        let nugetToken =
            Environment.GetEnvironmentVariable "MYGET_TOKEN"

        do! SyncService.run (DotnetNuget.pushToNuget nugetToken) db

        let rec whatForSync count =
            async {
                let! version = DotnetNuget.getLastVersion "y2k.nuget-test"

                let actual =
                    version
                    |> Option.defaultWith (fun _ -> failwith "Can't find package")
                    |> System.Version

                if expected <> actual then
                    if count = 0
                    then failwithf "Version in nuget not updated to %O" expected
                    do! Async.Sleep 5000
                    do! whatForSync (count - 1)
            }

        do! whatForSync 12
    }
    |> Async.RunSynchronously
