[@@@warning "-69"]

open Unix
module MapString = Map.Make (String)

type param =
  | NoParams
  | Json of Yojson.Safe.t

type request =
  { path : string
  ; verb : [ `GET | `POST | `DELETE | `PATCH | `POST | `PUT ]
  ; headers : string MapString.t
  ; body : Bytes.t option
  ; params : param
  }

let sock =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = setsockopt sock SO_REUSEADDR true in
  let _ = bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", 3000)) in
  let _ = listen sock 10 in
  sock
;;

let parse_headers ic =
  let rec helper acc =
    let line = In_channel.input_line ic in
    match line with
    | Some data -> if String.equal (String.trim data) "" then acc else helper (data :: acc)
    | None -> acc
  in
  helper [] |> List.rev
;;

let header_list_to_map string_list =
  let parse_header string =
    let sep_pos = String.index string ':' in
    let key = String.sub string 0 sep_pos |> String.lowercase_ascii in
    let vl =
      String.sub string (sep_pos + 2) (String.length string - String.length key - 2)
      |> String.trim
    in
    key, vl
  in
  List.map parse_header string_list |> MapString.of_list
;;

let verb_to_string = function
  | `GET -> "GET"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `POST -> "POST"
  | `PUT -> "PUT"
;;

let verb_of_string = function
  | "GET" -> `GET
  | "DELETE" -> `DELETE
  | "PATCH" -> `PATCH
  | "POST" -> `POST
  | "PUT " -> `PUT
  | _ -> failwith "Invalid verb"
;;

let headline_of_string string =
  let data = String.split_on_char ' ' string in
  match data with
  | [ verb; path; version ] -> verb_of_string verb, path, version
  | _ -> failwith "Invalid headline"
;;

let parse_request client =
  let ic = in_channel_of_descr client in
  match parse_headers ic with
  | [] -> failwith "Invalid request"
  | hd :: tl ->
    let verb, path, _version = headline_of_string hd in
    let headers = header_list_to_map tl in
    let content_length = MapString.find_opt "content-length" headers in
    let body =
      match content_length with
      | Some len ->
        let len = int_of_string len in
        let bitos = Bytes.create len in
        let _ = In_channel.input ic bitos 0 len in
        Some bitos
      | None -> None
    in
    let params =
      match verb, MapString.find_opt "content-type" headers, body with
      | `POST, Some "application/json", Some body ->
        Json (Yojson.Safe.from_string (Bytes.to_string body))
      | _ -> NoParams
    in
    { verb; path; headers; body; params }
;;

let log_request req =
  let time = Unix.gmtime (Unix.time ()) in
  Format.printf
    "%s %s - %02d:%02d:%02d GMT%!"
    (verb_to_string req.verb)
    req.path
    time.tm_hour
    time.tm_min
    time.tm_sec
;;

let handle_request request =
  match request.verb, request.path with
  | `GET, "/" -> "HTTP/1.1 200 OK\r\n\r\n<h1>oi</h1>"
  | _ -> "HTTP/1.1 404 NOT FOUND\r\n\r\n<h1>Page not found</h1>"
;;

let _ =
  let rec main () =
    let client, _ = accept sock in
    let request = parse_request client in
    let _ = log_request request in
    let response = handle_request request in
    let _ = write_substring client response 0 (String.length response) in
    let _ = close client in
    main ()
  in
  main ()
;;
