module Tests

open System
open Xunit
open Swensen.Unquote
open App
open Services
open MyGetBot

let private runTest f =
    async {
        let mkReducer db =
            Persistent.mkReducer MsgSerializer.info { items = Map.empty } Domain.reduce db

        let db =
            Persistent.make (new LiteDB.LiteDatabase(new IO.MemoryStream()))

        let log = ref []
        let mutable writeToBot = fun _ -> failwith "writeToBot not set"

        do! BotService.start (mkReducer db) (fun f ->
                writeToBot <-
                    (fun msg ->
                        log := []
                        f msg)
                async.Zero()) (fun _ msg ->
                log := msg :: !log
                async.Zero())

        do! f writeToBot log (mkReducer db)
    }
    |> Async.RunSynchronously

let private userAlice = User "0"
let private userBob = User "1"

[<Fact>]
let ``multi user test`` () =
    runTest
    <| fun writeToBot log _ ->
        async {
            do! writeToBot (userAlice, "/add https://github.com/alice/nuget-test/blob/master/nuget-test.fsproj")
            do! writeToBot (userBob, "/add https://github.com/bob/nuget-test/blob/master/nuget-test.fsproj")

            do! writeToBot (userBob, "/ls")
            Assert.Equal
                (box [ "Your subscriptions:\n- https://github.com/bob/nuget-test/blob/master/nuget-test.fsproj" ], !log)

            do! writeToBot (userAlice, "/ls")
            Assert.Equal
                (box [ "Your subscriptions:\n- https://github.com/alice/nuget-test/blob/master/nuget-test.fsproj" ],
                 !log)
        }

[<Fact>]
let ``no empty message for ls result`` () =
    runTest
    <| fun writeToBot log _ ->
        async {
            do! writeToBot (User "0", "/ls")
            Assert.Equal(1, List.length !log)
            Assert.All(!log, (fun x -> Assert.False(String.IsNullOrWhiteSpace x, sprintf "'%s' is empty" x)))
        }

[<Fact>]
let ``test bot commands ls`` () =
    runTest
    <| fun writeToBot log _ ->
        async {
            do! writeToBot (userAlice, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
            do! writeToBot (userAlice, "/ls")
            Assert.Equal
                (box [ "Your subscriptions:\n- https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj" ], !log)
        }

[<Fact>]
let ``test bot commands add`` () =
    runTest
    <| fun writeToBot log _ ->
        async {
            do! writeToBot (userAlice, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
            Assert.Equal(box [ "Operation successful" ], !log)
        }

[<Fact>]
let ``e2e test`` () =
    runTest
    <| fun writeToBot log reducer ->
        async {
            do! writeToBot (userAlice, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
            Assert.Equal(box [ "Operation successful" ], !log)

            let getAllReleases _ _ =
                [ "0.0.2", Uri "https://api.github.com/repos/y2k/nuget-test/zipball/0.0.2" ]
                |> Seq.ofList
                |> async.Return

            let nugetGetLastVersion _ = "0.0.1" |> Some |> async.Return

            let pushToNugetChannel =
                Threading.Channels.Channel.CreateBounded<string>(1)

            let pushToNuget path =
                (pushToNugetChannel.Writer.WriteAsync path).AsTask()
                |> Async.AwaitTask

            do! SyncService.run reducer nugetGetLastVersion getAllReleases pushToNuget

            let! path = pushToNugetChannel.Reader.ReadAsync()

            test <@ path.EndsWith "/bin/Release/y2k.nuget-test.0.0.2.nupkg" @>
        }

[<Fact>]
let ``save version must not be uploaded twice`` () =
    runTest
    <| fun writeToBot log reducer ->
        async {
            do! writeToBot (userAlice, "/add https://github.com/y2k/nuget-test/blob/master/nuget-test.fsproj")
            Assert.Equal(box [ "Operation successful" ], !log)

            let getAllReleases _ _ =
                [ "0.0.1", Uri "https://g.com/" ]
                |> Seq.ofList
                |> async.Return

            let nugetGetLastVersion _ = "0.0.1" |> Some |> async.Return

            do! SyncService.run reducer nugetGetLastVersion getAllReleases (fun _ ->
                    failwith "'pushToNuget' must not be called")
        }
