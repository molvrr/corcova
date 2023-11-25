module MapString = Map.Make (String)

type http_status =
  [ `Ok
  | `Created
  | `MovedPermanently
  | `NotFound
  ]

type body =
  | EmptyBody
  | String of string

type headers = string MapString.t
type cookies = string MapString.t

type t =
  { status : http_status
  ; body : body (* TODO: Criar função que abstrai as variants do body *)
  ; headers : headers
  ; cookies : cookies
  }

let empty =
  { status = `Ok; body = EmptyBody; headers = MapString.empty; cookies = MapString.empty }
;;

let body_of_response { body; _ } = body
let status { status; _ } = status

let body_to_string response =
  match response.body with
  | EmptyBody -> ""
  | String string -> string
;;

let set_status (response : t) ~status = { response with status }

let set_header (response : t) ~key ~value =
  let headers = response.headers |> MapString.add key value in
  { response with headers }
;;

let set_body (response : t) ~(body : body) =
  match body with
  | EmptyBody -> { response with body }
  | String string ->
    let response =
      set_header
        ~key:"Content-Length"
        ~value:(String.length string |> string_of_int)
        response
    in
    { response with body }
;;

let set_cookie response ~key ~value =
  let cookies = MapString.add key value response.cookies in
  { response with cookies }
;;

let redirect response path =
  response
  |> set_header ~key:"Location" ~value:path
  |> set_status ~status:`MovedPermanently
;;

let cookies_to_string response =
  let cookies = response.cookies in
  let cookies =
    MapString.fold (fun key value acc -> (key ^ "=" ^ value) :: acc) cookies []
  in
  String.concat ";" cookies
;;

let headers_to_string response =
  let headers = response.headers in
  let headers =
    MapString.fold (fun key value acc -> (key ^ ": " ^ value) :: acc) headers []
  in
  String.concat "\r\n" headers
;;

let status_to_http response =
  let version = "HTTP/1.1" in
  let status =
    match response.status with
    | `Ok -> "200 Ok"
    | `MovedPermanently -> "301 Moved Permanently"
    | `NotFound -> "404 Not Found"
    | `Created -> "201 Created"
  in
  version ^ " " ^ status ^ "\r\n"
;;

let to_string response =
  let cookies_length = response.cookies |> MapString.to_list |> List.length in
  let response =
    if cookies_length > 0
    then response |> set_header ~key:"Set-Cookie" ~value:(cookies_to_string response)
    else response
  in
  let status = status_to_http response in
  let headers = headers_to_string response in
  let body = body_to_string response in
  status ^ headers ^ "\r\n" ^ body
;;
