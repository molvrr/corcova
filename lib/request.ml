open Unix
module MapString = Map.Make (String)

type cookies = string MapString.t
type headers = string MapString.t

type param =
  | NoParams
  | Json of Yojson.Safe.t

type t =
  { path : string
  ; verb : [ `GET | `POST | `DELETE | `PATCH | `POST | `PUT ]
  ; headers : headers
  ; body : Bytes.t option
  ; params : param
  ; cookies : cookies option
  }

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

let parse_cookies = function
  | Some cookies ->
    let parse_cookie string =
      let sep_pos = String.index string ':' in
      let key = String.sub string 0 sep_pos |> String.lowercase_ascii in
      let vl =
        String.sub string (sep_pos + 2) (String.length string - String.length key - 2)
        |> String.trim
      in
      key, vl
    in
    let cookies = String.split_on_char ';' cookies in
    let cookies = List.map parse_cookie cookies in
    let cookies = MapString.of_list cookies in
    Some cookies
  | None -> None
;;

let of_fd client =
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
    let cookies = parse_cookies (MapString.find_opt "cookies" headers) in
    { verb; path; headers; body; params; cookies }
;;

let get_cookie request ~key =
  match request.cookies with
  | None -> None
  | Some cookies -> MapString.find_opt key cookies
;;
