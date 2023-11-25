[@@@warning "-20..70"]

open Unix
open Corcova

let sock =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = setsockopt sock SO_REUSEADDR true in
  let _ = bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", 3000)) in
  let _ = listen sock 10 in
  sock
;;

let handle_request routes (request : Request.t) =
  let open Response in
  let handler_opt =
    List.find_map
      (function
        | verb, path, handler when String.equal path request.path && request.verb = verb
          -> Some handler
        | _ -> None)
      routes
  in
  match handler_opt with
  | Some handler -> handler request empty
  | _ ->
    if String.equal request.path "/not_found"
    then empty |> set_status ~status:`NotFound |> render ~view:"views/404.html"
    else redirect ~path:"/not_found" empty
;;

(* TODO: Adicionar middleware *)
module Router = struct
  let get (path : string) (handler : Request.t -> Response.t -> Response.t) =
    `GET, path, handler
  ;;

  let post (path : string) (handler : Request.t -> Response.t -> Response.t) =
    `POST, path, handler
  ;;
end

module Routes = struct
  open Response
  open Request
  open Router

  let index =
    get "/" (fun req res ->
      match get_cookie ~key:"username" req with
      | Some user -> res |> render ~view:"views/index.html"
      | None -> res |> redirect ~path:"/login")
  ;;

  let login = get "/login" (fun req res -> res |> render ~view:"views/login.html")

  let post_login =
    post "/login" (fun req res ->
      res |> set_cookie ~key:"username" ~value:"anon" |> redirect ~path:"/")
  ;;

  let logout =
    post "/logout" (fun req res ->
      res
      |> set_cookie ~key:"username" ~value:""
      |> set_cookie ~key:"Max-Age" ~value:"0"
      |> redirect ~path:"/login")
  ;;

  let banana =
    let open View in
    get "/banana" (fun req res ->
      res |> render_html ~view:(html ~title:"Oi" ~body:[ p [] [ text "banana" ] ]))
  ;;
end

let router =
  [ Routes.index; Routes.login; Routes.post_login; Routes.logout; Routes.banana ]
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
