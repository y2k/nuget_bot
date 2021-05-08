module MyGetBot.EventStore

open LiteDB

let mkReducer
    (getId : 'at -> int, serialize : 'event -> 'at option, deserialize : 'at -> 'event)
    (init : 's)
    (f : 's -> 'event -> 's)
    (db : LiteDatabase)
    =
    let col = db.GetCollection<'at>("log")
    let index = ref 0
    let state = ref init

    { new IReducer<'s, 'event list> with
        member _.Invoke fxs =
            async {
                db.BeginTrans() |> ignore

                col.Find(Query.GT("_id", BsonValue.op_Implicit !index))
                |> Seq.iter
                    (fun a ->
                        let event = deserialize a
                        index := getId a
                        state := f !state event)

                let result, events = fxs !state

                for event in events do
                    state := f !state event

                    serialize event
                    |> Option.iter
                        (fun a ->
                            col.Insert a |> ignore
                            index := getId a)

                db.Commit() |> ignore

                return result
            } }
