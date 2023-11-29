open Unix
module View = View
module Request = Request
module Response = Response
module Middleware = Middleware

type handler = Middleware.handler

(* TODO: Função para aplicar middleware a uma única rota *)
type middleware = Middleware.t
type request_handler = Request.t -> Response.t -> Response.t

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

let compose (middleware_list : middleware list) =
  let id req resp = req, resp in
  List.fold_right (fun f acc -> f acc) middleware_list id
;;

let handle_request (routes : route list) (request : Request.t) =
  let open Response in
  let route_opt =
    List.find_opt
      (fun route -> route.verb = request.verb && String.equal request.path route.path)
      routes
  in
  match route_opt with
  | Some route ->
    let _, response =
      (compose
         (route.middlewares @ [ (fun _ req res -> req, route.handler req res) ])
         request)
        empty
    in
    response
  | None -> empty |> set_status ~status:`NotFound
;;

module Router = struct
  let scope ?(prefix = "") (middleware_list : middleware list) routes =
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

  module Utils = struct
    let path { path; _ } = path

    let verb { verb; _ } =
      match verb with
      | `GET -> "GET"
      | `POST -> "POST"
      | `DELETE -> "DELETE"
      | `PUT -> "PUT"
      | `PATCH -> "PATCH"
    ;;
  end

  (** Add debug routes. *)
  module Debug = struct
    open Response
    open View

    let requests_counter = ref 0

    let counter_middleware : middleware =
      fun next_handler request ->
      let () = requests_counter := !requests_counter + 1 in
      next_handler request
    ;;

    let routes_index routes =
      let compose arg = Utils.verb arg ^ " " ^ Utils.path arg in
      let routes =
        List.map compose routes
        |> List.map txt
        |> List.map (fun route -> div [] [ route ])
      in
      let body = [ div [] routes ] in
      let view = html "routes" body in
      get "/routes" (fun _req res -> res |> set_body ~body:(Html view))
    ;;

    let counter_page =
      get "/stats" (fun _req res ->
        res
        |> set_body ~body:(String ("<h1>" ^ string_of_int !requests_counter ^ "</h1>")))
    ;;

    let index =
      get "/" (fun _req res ->
        res |> set_body ~body:(String {|<a href="/debug/stats"">Stats</a>|}))
    ;;

    let make routes =
      scope ~prefix:"/debug" [] [ index; routes_index routes; counter_page ]
      @ scope [ counter_middleware ] routes
    ;;
  end
end

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

let motd =
  {|
   _____                              
  / ____|                             
 | |     ___  _ __ ___ _____   ____ _ 
 | |    / _ \| '__/ __/ _ \ \ / / _` |
 | |___| (_) | | | (_| (_) \ V / (_| |
  \_____\___/|_|  \___\___/ \_/ \__,_|

                                                  
                    λλλλ   λλλλλ           
      λλλλ        λλλλλλλλλλλλλλλλ         
  λλλλλλλλλλ     λλλλλλλλλλλλλλλλλλ        
  λλλλλλλλλλλ λλλλλλλλλλλλλλλλλλλλλλ       
     λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ     
     λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ    
      λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ   
         λλλλλλλλλλλλλλλλλλλλλλλλλλλλλ     
             λλλλλλλλ         λλλλ λλλλλ   
             λλλλ λλλ         λλλλ   λλλ   
              λλλλλλλ        λλλλ    λλλ   
                 λλλλ     λλλλλ       λλ   
                 λλλ      λλλ        λλ    
|}
;;

let run app =
  let () = Format.printf "%s\nListening on port %d...\n%!" motd app.port in
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
