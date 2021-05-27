module MyGetBot.MsgSerializer

[<CLIMutable>]
type t =
    { tag: int
      user: string
      url: string }

let serialize =
    function
    | SyncRequested _ -> None
    | UrlAdded (User user, Url url) ->
        { tag = 1
          user = user
          url = url }
        |> Some
    | UrlRemoved (User user, Url url) ->
        { tag = 2
          user = user
          url = url }
        |> Some

let deserialize =
    function
    | { tag = 1; user = user; url = url } -> UrlAdded(User user, Url url)
    | { tag = 2; user = user; url = url } -> UrlRemoved(User user, Url url)
    | value -> failwithf "Can't parse %O" value
