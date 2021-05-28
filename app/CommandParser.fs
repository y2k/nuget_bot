module CommandParser

type Param =
    | StringParam of name: string
    | IntParam of name: string

type 't FuncWrapper =
    abstract member Invoke : obj [] -> 't

type 't RuleParams =
    | Name of string
    | Description of string
    | Param of Param
    | Callback of FuncWrapper<'t>
    | Return of 't Lazy

type 't Rule = Rule of 't RuleParams list

let OnCallback f =
    { new FuncWrapper<'t> with
        member _.Invoke xs = f (unbox <| xs.[0]) }
    |> Callback

let OnCallback2 f =
    { new FuncWrapper<'t> with
        member _.Invoke xs = f (unbox <| xs.[0]) (unbox <| xs.[1]) }
    |> Callback

let OnCallback3 f =
    { new FuncWrapper<'t> with
        member _.Invoke xs =
            f (unbox <| xs.[0]) (unbox <| xs.[1]) (unbox <| xs.[2]) }
    |> Callback

let eval (text: string) (rules: 't Rule list) : Result<'t, string> =
    let renderErrorHelp () =
        rules
        |> List.map
            (fun (Rule ps) ->
                let name =
                    List.pick
                        (function
                        | Name n -> Some n
                        | _ -> None)
                        ps

                let desc =
                    ps
                    |> List.tryPick
                        (function
                        | Description n -> Some n
                        | _ -> None)
                    |> Option.defaultValue "no description"

                let args =
                    ps
                    |> List.choose
                        (function
                        | Param (StringParam n) -> Some(sprintf "<%s : string>" n)
                        | Param (IntParam n) -> Some(sprintf "<%s : int>" n)
                        | _ -> None)
                    |> function
                    | [] -> [ "" ]
                    | xs -> xs
                    |> List.reduce (sprintf "%s %s")
                    |> function
                    | "" -> ""
                    | s -> " " + s

                sprintf "/%s%s - %s" name args desc)
        |> List.reduce (sprintf "%s\n%s")
        |> Error

    let parts = text.Split " "

    let r =
        rules
        |> List.tryFind
            (fun (Rule ps) ->
                let name =
                    List.pick
                        (function
                        | Name n -> Some n
                        | _ -> None)
                        ps

                ("/" + name) = parts.[0])

    match r with
    | Some (Rule ps) ->
        let result =
            ps
            |> List.tryPick
                (function
                | Return f -> Some f
                | _ -> None)

        match result with
        | Some result -> Ok(result.Value)
        | None ->
            let argTypes =
                ps
                |> List.choose
                    (function
                    | Param f -> Some f
                    | _ -> None)

            if parts.Length - 1 < argTypes.Length then
                renderErrorHelp ()
            else
                let f =
                    ps
                    |> List.pick
                        (function
                        | Callback f -> Some f
                        | _ -> None)

                parts
                |> Array.tail
                |> Array.zip (List.toArray argTypes)
                |> Array.map
                    (function
                    | StringParam _, v -> box v
                    | IntParam _, v -> int v |> box)
                |> f.Invoke
                |> Ok
    | None -> renderErrorHelp ()
