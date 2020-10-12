[<AutoOpen>]
module Prelude

open System

module Async =
    let catch a =
        async {
            let! x = a |> Async.Catch
            return
                match x with
                | Choice1Of2 x -> Ok x
                | Choice2Of2 e -> Error e
        }

type Microsoft.FSharp.Control.AsyncBuilder with
    member __.Bind (t : Threading.Tasks.ValueTask<'T>, f:'T -> Async<'R>) : Async<'R> =
        async.Bind(Async.AwaitTask <| t.AsTask(), f)
