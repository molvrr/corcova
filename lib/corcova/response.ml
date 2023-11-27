type http_status =
  [ `Ok
  | `Created
  | `MovedPermanently
  | `NotFound
  | `BadRequest
  ]

type body =
  | EmptyBody
  | String of string
  | Static of string
  | Html of View.t

type headers = (string * string) list

type cookie_data =
  { value : string
  ; attributes : (string * string) list
  }

type cookie = string * cookie_data
type cookies = cookie list

type t =
  { status : http_status
  ; body : body (* TODO: Criar função que abstrai as variants do body *)
  ; headers : headers
  ; cookies : cookies
  }

let empty = { status = `Ok; body = EmptyBody; headers = []; cookies = [] }
let body_of_response { body; _ } = body
let status { status; _ } = status

let body_to_string response =
  match response.body with
  | EmptyBody -> ""
  | String string -> string
  | Static filename -> In_channel.input_all @@ open_in filename
  | Html html -> View.to_string html
;;

let set_status (response : t) ~status = { response with status }

let set_header (response : t) ~key ~value =
  let headers = (key, value) :: response.headers in
  { response with headers }
;;

let set_body (response : t) ~(body : body) =
  (* NOTE: Só definir Content-Length na hora renderizar? *)
  match body with
  | EmptyBody -> { response with body }
  | Static filename ->
    let response =
      response
      |> set_header
           ~key:"Content-Length"
           ~value:(string_of_int @@ (Unix.stat filename).st_size)
    in
    { response with body }
  | String string ->
    let response =
      set_header
        ~key:"Content-Length"
        ~value:(String.length string |> string_of_int)
        response
    in
    { response with body }
  | Html html ->
    let response =
      set_header
        ~key:"Content-Length"
        ~value:(string_of_int @@ String.length (View.to_string html))
        response
    in
    { response with body }
;;

(* NOTE: Aceitar apenas atributos específicos pode ser uma ideia melhor. *)
let set_cookie ?(attributes : (string * string) list = []) response ~key ~value =
  let cookies = (key, { value; attributes }) :: response.cookies in
  { response with cookies }
;;

let redirect response ~path =
  response
  |> set_header ~key:"Location" ~value:path
  |> set_status ~status:`MovedPermanently
;;

let cookie_to_string ((key, data) : cookie) =
  let attrs = List.map (fun (k, v) -> k ^ "=" ^ v) data.attributes in
  if List.length attrs > 0
  then key ^ "=" ^ data.value ^ ";" ^ String.concat ";" attrs
  else key ^ "=" ^ data.value
;;

let headers_to_string response =
  let headers = response.headers in
  let headers =
    List.fold_left (fun acc (key, value) -> (key ^ ": " ^ value) :: acc) [] headers
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
    | `BadRequest -> "400 Bad Request"
  in
  version ^ " " ^ status ^ "\r\n"
;;

let to_string response =
  let cookies_length = response.cookies |> List.length in
  let response =
    if cookies_length > 0
    then
      List.fold_left
        (fun response value -> set_header ~key:"Set-Cookie" ~value response)
        response
        (List.map cookie_to_string response.cookies)
    else response
  in
  let headers_length = response.headers |> List.length in
  let status = status_to_http response in
  let headers = headers_to_string response in
  let body = body_to_string response in
  if headers_length > 0
  then status ^ headers ^ "\r\n" ^ "\r\n" ^ body
  else status ^ "\r\n"
;;

let render response ~view =
  response
  |> set_header ~key:"Content-Type" ~value:"text/html"
  |> set_body ~body:(Static view)
;;

let render_html response ~view =
  response
  |> set_header ~key:"Content-Type" ~value:"text/html"
  |> set_body ~body:(Html view)
;;

let render_json response ~view =
  let json = Yojson.Safe.to_string view in
  response
  |> set_header ~key:"Content-Type" ~value:"application/json"
  |> set_body ~body:(String json)
;;

let bad_request response = response |> set_status ~status:`BadRequest
