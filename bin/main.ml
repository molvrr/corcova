open Corcova

module Routes = struct
  open Response
  open Request
  open Router

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
    get "/banana" (fun _req res ->
      res
      |> render_html
           ~view:
             (html
                ~title:"Oi"
                ~body:
                  [ p
                      []
                      [ img
                          ~src:
                            "https://t4.ftcdn.net/jpg/00/53/03/69/360_F_53036913_Fn9JosHqTi97KQJ40q4i4zgimPhJhLAm.jpg"
                          ~alt:"Uma banana"
                      ]
                  ]))
  ;;

  module Api = struct
    let user =
      get "/user" (fun _req res ->
        res |> set_body ~body:(String {|{"name": "AAAAAAAA"}|}))
    ;;
  end
end

(*
   GET /
   GET /login
   POST /login
   POST /logout
   GET /banana
   SCOPE     /api
   ---- POST /user
*)

let routes : route list =
  Router.scope
    Corcova.Middleware.[ logger; no_cache ]
    [ Routes.index
    ; Routes.login
    ; Routes.post_login
    ; Routes.logout
    ; Routes.banana
    ; Routes.alt
    ]
  @ Router.scope
      ~prefix:"/api"
      Corcova.Middleware.[ logger; json; no_cache ]
      [ Routes.Api.user ]
  |> Router.Debug.make
;;

let () = Corcova.empty |> Router.add_routes ~routes |> Corcova.run
