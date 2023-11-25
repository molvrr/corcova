open Unix
open Corcova

type handler = Request.t -> Request.t
type request_handler = Request.t -> Response.t -> Response.t
type middleware = handler -> handler

type route =
  { verb : [ `DELETE | `GET | `PATCH | `POST | `PUT ]
  ; path : string
  ; middlewares : middleware list
  ; handler : request_handler
  }

let sock =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = setsockopt sock SO_REUSEADDR true in
  let _ = bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", 3000)) in
  let _ = listen sock 10 in
  sock
;;

let json : middleware =
  let open Request in
  fun handler req ->
    let _headers = req.headers in
    let is_json =
      Request.get_header req "content-type"
      |> Option.map (String.equal "application/json")
      |> Option.value ~default:false
    in
    let params =
      if is_json
      then (
        match req.body with
        | Some body -> Json (Yojson.Safe.from_string (Bytes.to_string body))
        | None -> NoParams)
      else req.params
    in
    handler { req with params }
;;

let compose (middleware_list : middleware list) =
  List.fold_left (fun acc f -> f acc) Fun.id middleware_list
;;

let handle_request (routes : route list) (request : Request.t) =
  let open Response in
  let route_opt =
    List.find_opt
      (fun route -> route.verb = request.verb && String.equal request.path route.path)
      routes
  in
  match route_opt with
  | Some route -> route.handler (compose route.middlewares request) empty
  | _ ->
    if String.equal request.path "/not_found"
    then empty |> set_status ~status:`NotFound |> render ~view:"views/404.html"
    else redirect ~path:"/not_found" empty
;;

module Router = struct
  let scope prefix (middleware_list : middleware list) routes =
    List.map
      (fun route ->
        { route with
          path = prefix ^ route.path
        ; middlewares = middleware_list @ route.middlewares
        })
      routes
  ;;

  let get (path : string) (handler : Request.t -> Response.t -> Response.t) : route =
    { verb = `GET; middlewares = []; handler; path }
  ;;

  let post (path : string) (handler : Request.t -> Response.t -> Response.t) : route =
    { verb = `POST; middlewares = []; handler; path }
  ;;
end

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
      res |> render_html ~view:(html ~title:"Oi" ~body:[ p [] [ text "banana" ] ]))
  ;;

  module Api = struct
    let user =
      post "/user" (fun req res ->
        let _params =
          (function
            | Json json -> json
            | NoParams -> failwith "Sem json")
            req.params
        in
        res
        |> set_body ~body:(String {|{"name": "Mateus"}|})
        |> set_header ~key:"Content-Type" ~value:"application/json")
    ;;
  end
end

let router : route list =
  [ Routes.index; Routes.login; Routes.post_login; Routes.logout; Routes.banana ]
  @ Router.scope "/api" [ json ] [ Routes.Api.user ]
;;

let send response client =
  let open Response in
  let response = to_string response in
  let _ = write_substring client response 0 (String.length response) in
  ()
;;

let _ =
  let rec main () =
    let client, _ = accept sock in
    let request = Request.of_fd client in
    let response = handle_request router request in
    let _ = send response client in
    let _ = close client in
    main ()
  in
  main ()
;;
