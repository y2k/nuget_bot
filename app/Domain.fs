namespace MyGetBot

type User = User of string
type Url = Url of string

type State =
    { items: Map<User, Url Set>
      syncRequested: bool }
    static member Empty =
        { items = Map.empty
          syncRequested = false }

type Msg =
    | SyncRequested of bool
    | UrlAdded of user: User * url: Url
    | UrlRemoved of user: User * url: Url

type IReducer<'state, 'events> =
    abstract member Invoke : ('state -> 'r * 'events) -> 'r Async

module ParseMsg =
    type t =
        | Add of string
        | Ls
        | Sync
        | Unknown

    let parseMessage (msg: string) : t =
        match msg.Split ' ' |> List.ofSeq with
        | [ "/add"; url ] -> Add url
        | [ "/ls" ] -> Ls
        | [ "/sync" ] -> Sync
        | _ -> Unknown

module MessageGenerator =
    let startMessage = """
Commands (ver 0.2):
/ls - list of packages
/add <github url to (f|c)proj> - add package
/sync - sync your repositories"""

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
        |> replace "(<PropertyGroup>)" $"$1<PackageId>%s{user}.%s{repo}</PackageId>"
        |> replace "<Version>.+?</Version>" ""
        |> replace "(<PropertyGroup>)" $"$1<Version>%s{version}</Version>"

    let getAllUrl items =
        items
        |> Map.toList
        |> List.collect (fun (_, urls) -> urls |> Set.toList)

    let reduce state =
        function
        | SyncRequested r -> { state with syncRequested = r }
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
        match url with
        | Regex "github\\.com/(.+?)/(.+?)/blob/master/(.+\\.fsproj)$" [ user; repo; proj ] ->
            {| user = user
               repo = repo
               proj = proj |}
            |> Some
        | _ -> None

    let getNugetId githubUrl =
        parseUrl githubUrl
        |> Option.map (fun x -> $"%s{x.user}.%s{x.repo}")

    let private tryAddUrl user (_: State) url : Msg list =
        parseUrl url
        |> Option.map (fun _ -> [ UrlAdded(user, url) ])
        |> Option.defaultValue []

    let handleMsg (user, msg) (state: State) : string * Msg list =
        match ParseMsg.parseMessage msg with
        | ParseMsg.Sync -> "Sync scheduled", [ SyncRequested true ]
        | ParseMsg.Add url -> MessageGenerator.formatLsMessage, tryAddUrl user state (Url url)
        | ParseMsg.Ls -> MessageGenerator.formatMessage user state.items, []
        | ParseMsg.Unknown -> MessageGenerator.startMessage, []
