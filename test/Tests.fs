module Tests

open System
open Xunit
open App
open Swensen.Unquote

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

        let getAllReleases _ _ =
            [ { Github.Release.version = "0.0.2"
                Github.Release.zipUrl = "https://api.github.com/repos/y2k/nuget-test/zipball/0.0.2" } ]
            |> Seq.ofList
            |> async.Return

        let nugetGetLastVersion _ = "0.0.1" |> Some |> async.Return

        let pushToNugetChannel =
            Threading.Channels.Channel.CreateBounded<string>(1)

        let pushToNuget path =
            (pushToNugetChannel.Writer.WriteAsync path).AsTask()
            |> Async.AwaitTask

        do! SyncService.run nugetGetLastVersion getAllReleases pushToNuget db

        let! path =
            pushToNugetChannel.Reader.ReadAsync().AsTask()
            |> Async.AwaitTask

        test <@ path.EndsWith "/bin/Release/y2k.nuget-test.0.0.2.nupkg" @>
    }
    |> Async.RunSynchronously

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

        let getAllReleases _ _ =
            [ { Github.Release.version = "0.0.1"
                Github.Release.zipUrl = "" } ]
            |> Seq.ofList
            |> async.Return

        let nugetGetLastVersion _ = "0.0.1" |> Some |> async.Return

        do! SyncService.run nugetGetLastVersion getAllReleases (fun _ -> failwith "'pushToNuget' must not be called") db
    }
    |> Async.RunSynchronously
