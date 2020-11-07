module PersistentTests

open System.IO
open Xunit
open Swensen.Unquote
open App
open Services

[<Fact>]
let test () =
    use liteDb =
        new LiteDB.LiteDatabase(new MemoryStream())

    let db = Persistent.make (liteDb)

    async {
        let reducer =
            Persistent.mkReducer MsgSerializer.info { items = Map.empty } Domain.reduce db

        let! _ = reducer (fun _ -> [ UrlAdded(User "user1", Url "url1") ])

        let! actual = reducer (fun _ -> [])

        let expected =
            { items = Map.ofList [ User "user1", Set.singleton (Url "url1") ] }

        test <@ actual = expected @>

        let reducer =
            Persistent.mkReducer MsgSerializer.info { items = Map.empty } Domain.reduce db

        let! actual = reducer (fun _ -> [])

        test <@ actual = expected @>
    }
    |> Async.RunSynchronously