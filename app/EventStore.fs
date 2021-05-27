module MyGetBot.EventStore

open LiteDB

type 'msg State = { queue: 'msg list }

let empty : 'msg State = { queue = [] }

let merge s e = { s with queue = e :: s.queue }

let restore (db: LiteDatabase) (deserialize: 'json -> 'event) reduce =
    async {
        let col = db.GetCollection<'json>("log")

        let events =
            col.FindAll() |> Seq.map deserialize |> Seq.toList

        do! reduce (fun db -> db, events) |> Async.Ignore
    }

let dumpEvents (db: LiteDatabase) (serialize: 'event -> 'json option) reduce =
    async {
        while true do
            let col = db.GetCollection<'json>("log")

            db.BeginTrans() |> ignore

            let! (store: _ State) = reduce (fun db -> empty, [])

            for event in store.queue |> List.rev do
                let x = serialize event

                match x with
                | Some a -> col.Insert a |> ignore
                | None -> ()

            db.Commit() |> ignore

            do! Async.Sleep 5_000
    }
