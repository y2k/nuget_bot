namespace MyGetBot

type User = User of string
type Url = Url of string
type State =
    { items: Map<User, Url Set>; syncRequested : bool }
    with static member Empty = { items = Map.empty; syncRequested = false }
type Msg =
    | UrlAdded of user: User * url: Url
    | UrlRemoved of user: User * url: Url

module ParseMsg =
    type t =
        | Start
        | Add of string
        | Ls
        | Sync
        | Unknown

    let parseMessage (msg: string): t =
        match msg.Split ' ' |> List.ofSeq with
        | [ "/add"; url ] -> Add url
        | [ "/ls" ] -> Ls
        | [ "/start" ] -> Start
        | [ "/sync" ] -> Sync
        | _ -> Unknown

module MessageGenerator =
    let startMessage =
        "Commands (ver 0.1):\n/ls - list of packages\n/add <github url to (f|c)proj> - add package"

    let formatMessage user items =
        items
        |> Map.tryFind user
        |> Option.defaultValue Set.empty
        |> Set.toList
        |> List.map (fun (Url url) -> url)
        |> List.fold (fun a x -> a + "\n- " + x) ""
        |> fun x -> sprintf "Your subscriptions:\n%s" <| x.Trim '\n'

    let formatLsMessage = "Operation successful"

module Domain =
    open System.Text.RegularExpressions

    let updateProj user repo version xml =
        let replace s (r: string) xml = Regex.Replace(xml, s, r)
        xml
        |> replace "<PackageId>.+?</PackageId>" ""
        |> replace "(<PropertyGroup>)" (sprintf "$1<PackageId>%s.%s</PackageId>" user repo)
        |> replace "<Version>.+?</Version>" ""
        |> replace "(<PropertyGroup>)" (sprintf "$1<Version>%s</Version>" version)

    let getAllUrl items =
        items
        |> Map.toList
        |> List.collect (fun (_, urls) -> urls |> Set.toList)

    let reduce state =
        function
        | UrlAdded (user, url) ->
            let urls =
                Map.tryFind user state.items
                |> Option.defaultValue Set.empty
                |> Set.add url

            { state with
                  items = Map.add user urls state.items }
        | UrlRemoved (user, url) ->
            let urls =
                Map.tryFind user state.items
                |> Option.defaultValue Set.empty
                |> Set.remove url

            { state with
                  items = Map.add user urls state.items }

    let parseUrl (Url url) =
        match Regex.Match(url, "github\\.com/(.+?)/(.+?)/blob/master/(.+\\.fsproj)$").Groups
              |> Seq.toList with
        | [ _; a; b; c ] ->
            Some
                {| user = a.Value
                   repo = b.Value
                   proj = c.Value |}
        | _ -> None

    let getNugetId githubUrl =
        parseUrl githubUrl
        |> Option.map (fun x -> sprintf "%s.%s" x.user x.repo)

    let private tryAddUrl user (_: State) url: Msg list =
        parseUrl url
        |> Option.map (fun _ -> [ UrlAdded(user, url) ])
        |> Option.defaultValue []

    let handleMsg (user, msg) (state: State): Msg list * string =
        match ParseMsg.parseMessage msg with
        | ParseMsg.Sync -> failwith "???"
        | ParseMsg.Add url -> tryAddUrl user state (Url url), MessageGenerator.formatLsMessage
        | ParseMsg.Ls -> [], MessageGenerator.formatMessage user state.items
        | ParseMsg.Start -> [], MessageGenerator.startMessage
        | ParseMsg.Unknown -> [], sprintf "Unknown command: %s" msg
