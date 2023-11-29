open Corcova
open Router

module Routes : sig
  val all : route list
end = struct
  open Request
  open Response

  let index =
    get "/" (fun req res ->
      match get_cookie ~key:"username" req with
      | Some _user -> res |> render ~view:(Views.User.index req)
      | None -> res |> redirect ~path:"/login")
  ;;

  let login =
    get "/login" (fun req res ->
      match get_cookie ~key:"username" req with
      | Some _ -> res |> redirect ~path:"/"
      | None -> render ~view:(Views.User.login req) res)
  ;;

  let post_login =
    post "/login" (fun _req res ->
      res |> set_cookie ~key:"username" ~value:"anon" |> redirect ~path:"/")
  ;;

  let logout =
    post "/logout" (fun _req res ->
      res
      |> set_cookie ~key:"username" ~value:"" ~attributes:[ "Max-Age", "0" ]
      |> redirect ~path:"/login")
  ;;

  let banana = get "/banana" (fun _req res -> res |> render ~view:Views.User.banana)

  module Api = struct
    let user =
      get "/user" (fun _req res ->
        res
        |> set_header ~key:"Content-Type" ~value:"application/json"
        |> set_body ~body:(String {|{"name": "AAAAAAAA"}|}))
    ;;
  end

  let skip_unless_authenticated : middleware =
    fun next_handler req res ->
    match Request.get_cookie ~key:"username" req with
    | None ->
      let res = redirect ~path:"/" res in
      req, res
    | Some _ -> next_handler req res
  ;;

  let all : route list =
    Router.scope
      Corcova.Middleware.[ logger; no_cache ]
      [ index; login; post_login; logout; banana ]
    @ Router.scope
        ~prefix:"/api"
        Corcova.Middleware.[ logger; json; no_cache; skip_unless_authenticated ]
        [ Api.user ]
    |> Router.Debug.make
  ;;
end

let () = empty |> add_routes ~routes:Routes.all |> run
