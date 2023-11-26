open Unix
module View = View
module Request = Request
module Response = Response

type handler = Request.t -> Request.t
type request_handler = Request.t -> Response.t -> Response.t
type middleware = handler -> handler

type route =
  { verb : [ `DELETE | `GET | `PATCH | `POST | `PUT ]
  ; path : string
  ; middlewares : middleware list
  ; handler : request_handler
  }

type t =
  { port : int
  ; host : string
  ; routes : route list
  }

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
  List.fold_right (fun f acc -> f acc) middleware_list Fun.id
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

  let add_routes app ~routes =
    List.fold_left (fun app route -> { app with routes = route :: app.routes }) app routes
  ;;
end

let logger : middleware =
  fun next_handler req ->
  let time = Unix.time () |> Unix.gmtime in
  let () =
    Format.printf
      "%02d:%02d:%02d - %s %s\n%!"
      time.tm_hour
      time.tm_min
      time.tm_sec
      ((function
         | `GET -> "GET"
         | `POST -> "POST"
         | `PATCH -> "PATCH"
         | `PUT -> "PUT"
         | `DELETE -> "DELETE")
         req.verb)
      req.path
  in
  next_handler req
;;

let send response client =
  let open Response in
  let response = to_string response in
  let _ = write_substring client response 0 (String.length response) in
  ()
;;

let empty = { port = 3000; host = "0.0.0.0"; routes = [] }
let set_host app ~host = { app with host }
let set_port app ~port = { app with port }

let socket app =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = setsockopt sock SO_REUSEADDR true in
  let _ = bind sock (ADDR_INET (inet_addr_of_string app.host, app.port)) in
  let _ = listen sock 10 in
  sock
;;

let run app =
  let sock = socket app in
  let rec main () =
    let client, _ = accept sock in
    let request = Request.of_fd client in
    let response = handle_request app.routes request in
    let _ = send response client in
    let _ = close client in
    main ()
  in
  main ()
;;
