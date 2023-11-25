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
  | _ -> empty |> set_status ~status:`NotFound
;;

(* TODO: Adicionar middleware *)
module Router = struct
  let get (path : string) (handler : Request.t -> Response.t -> Response.t) =
    `GET, path, handler
  ;;
end

module Routes = struct
  open Response
  open Router

  module Get = struct
    let index =
      get "/" (fun _req res ->
        res
        |> set_body ~body:(String "<h1>Oi</h1>")
        |> set_cookie ~key:"username" ~value:"beeeeep")
    ;;
  end
end

let router = [ Routes.Get.index ]

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
