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
      | Some _user -> res |> render ~view:"views/index.html"
      | None -> res |> redirect ~path:"/login")
  ;;

  let alt = get "/alt" (fun _req res -> res |> set_body ~body:(Html Views.User.index))

  let login =
    get "/login" (fun req res ->
      match get_cookie ~key:"username" req with
      | Some _ -> res |> redirect ~path:"/"
      | None -> render ~view:"views/login.html" res)
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

  let banana =
    let open View in
    let src =
      "https://t4.ftcdn.net/jpg/00/53/03/69/360_F_53036913_Fn9JosHqTi97KQJ40q4i4zgimPhJhLAm.jpg"
    in
    get "/banana" (fun _req res ->
      res
      |> render_html
           ~view:(html ~title:"Oi" ~body:[ p [] [ img ~src ~alt:"Uma banana" ] ]))
  ;;

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
      [ index; login; post_login; logout; banana; alt ]
    @ Router.scope
        ~prefix:"/api"
        Corcova.Middleware.[ logger; json; no_cache; skip_unless_authenticated ]
        [ Api.user ]
    |> Router.Debug.make
  ;;
end

let () = empty |> add_routes ~routes:Routes.all |> run
