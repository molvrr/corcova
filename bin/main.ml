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

  let login = get "/login" (fun _req res -> res |> render ~view:"views/login.html")

  let post_login =
    post "/login" (fun _req res ->
      res |> set_cookie ~key:"username" ~value:"anon" |> redirect ~path:"/")
  ;;

  let logout =
    post "/logout" (fun _req res ->
      res
      |> set_cookie ~key:"username" ~value:""
      |> set_cookie ~key:"Max-Age" ~value:"0"
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
      post "/user" (fun req res ->
        let _params =
          (function
            | Json json -> Some json
            | NoParams -> None)
            req.params
        in
        res
        |> set_body ~body:(String {|{"name": "AAAAAAAA"}|})
        |> set_header ~key:"Content-Type" ~value:"application/json")
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
  [ Routes.index; Routes.login; Routes.post_login; Routes.logout; Routes.banana ]
  @ Router.scope ~prefix:"/api" [ logger; json ] [ Routes.Api.user ]
  |> Router.Debug.make
;;

let () = Corcova.empty |> Router.add_routes ~routes |> Corcova.run
