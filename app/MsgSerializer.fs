module MyGetBot.MsgSerializer

[<CLIMutable>]
type t =
    { id : int
      tag : int
      user : string
      url : string }

let private getId x = x.id

let private serialize =
    function
    | SyncRequested _ -> None
    | UrlAdded (User user, Url url) ->
        { id = 0
          tag = 1
          user = user
          url = url }
        |> Some
    | UrlRemoved (User user, Url url) ->
        { id = 0
          tag = 2
          user = user
          url = url }
        |> Some

let private deserialize =
    function
    | { tag = 1; user = user; url = url } -> UrlAdded(User user, Url url)
    | { tag = 2; user = user; url = url } -> UrlRemoved(User user, Url url)
    | value -> failwithf "Can't parse %O" value

let info = getId, serialize, deserialize
