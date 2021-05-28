namespace MyGetBot

module Result =
    let inline fold fOk fError =
        function
        | Ok x -> fOk x
        | Error e -> fError e

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

module MessageGenerator =
    let formatMessage user items =
        items
        |> Map.tryFind user
        |> Option.defaultValue Set.empty
        |> Set.toList
        |> List.map (fun (Url url) -> url)
        |> List.fold (fun a x -> a + "\n- " + x) ""
        |> fun x -> sprintf "Your subscriptions:\n%s" <| x.Trim '\n'

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

    module P = CommandParser

    let handleMsg (user, msg) (state: State) =
        [ P.Rule [ P.Name "sync"
                   P.Description "Schedule sync your repositories"
                   P.Return(lazy ("Sync scheduled", [ SyncRequested true ])) ]
          P.Rule [ P.Name "ls"
                   P.Description "Show list your of packages"
                   P.Return(lazy (MessageGenerator.formatMessage user state.items, [])) ]
          P.Rule [ P.Name "add"
                   P.Description "Add github url to (f|c)proj package"
                   P.Param <| P.StringParam "url"
                   P.OnCallback(fun url -> "Operation successful", tryAddUrl user state (Url url)) ] ]
        |> P.eval msg
        |> Result.fold id (fun msg -> sprintf "Commands (ver 0.2):\n%s" msg, [])
