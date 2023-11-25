open Unix
module MapString = Map.Make (String)

type request =
  { path : string
  ; verb : [ `GET | `POST | `DELETE | `PATCH | `POST | `PUT ]
  ; headers : string MapString.t
  ; body : Bytes.t option
  }

let sock =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = setsockopt sock SO_REUSEADDR true in
  let _ = bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", 3000)) in
  let _ = listen sock 10 in
  sock
;;

let client, _ = accept sock

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
    let content_type = MapString.find_opt "content-type" headers in
    let body =
      match content_type with
      | Some len ->
        let bitos = Bytes.create (int_of_string len) in
        let _ = read client bitos 0 (int_of_string len) in
        Some bitos
      | None -> None
    in
    { verb; path; headers; body }
;;

let pp_request req =
  let time = Unix.gmtime (Unix.time ()) in
  Format.printf
    "%s %s - %02d:%02d:%02d GMT"
    (verb_to_string req.verb)
    req.path
    time.tm_hour
    time.tm_min
    time.tm_sec
;;

let _ =
  let request = parse_request client in
  let _ = request.headers in
  let _ = request.body in
  pp_request request
;;
