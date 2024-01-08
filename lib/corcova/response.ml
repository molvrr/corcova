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
  | Html of Tyxml_html.doc
  | Json of Yojson.Safe.t
  | Static of string

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
  | Html html -> Format.asprintf "%a" (Tyxml.Html.pp ()) html
  | Json json -> Yojson.Safe.to_string json
  | Static filename ->
    let ic = open_in filename in
    let file_data = In_channel.input_all ic in
    let () = close_in ic in
    file_data
;;

let set_status (response : t) ~status = { response with status }

let set_header (response : t) ~key ~value =
  let headers = (key, value) :: response.headers in
  { response with headers }
;;

let set_body (response : t) ~(body : body) =
  (* FIXME: Só definir Content-Length na hora renderizar? *)
  match body with
  | EmptyBody -> { response with body }
  | Json json ->
    let response =
      set_header
        ~key:"Content-Length"
        ~value:(String.length @@ Yojson.Safe.to_string json |> string_of_int)
        response
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
        ~value:
          (string_of_int @@ String.length (Format.asprintf "%a" (Tyxml.Html.pp ()) html))
        response
    in
    { response with body }
  | Static filename ->
    let string =
      let ic = open_in filename in
      let file_data = In_channel.input_all ic in
      let () = close_in ic in
      file_data
    in
    let response =
      set_header
        ~key:"Content-Length"
        ~value:(String.length string |> string_of_int)
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
  then Format.sprintf "%s=%s;%s" key data.value @@ String.concat ";" attrs
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
  Format.asprintf "%s %s\r\n" version status
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
  then Format.asprintf "%s%s\r\n\r\n%s" status headers body
  else status ^ "\r\n"
;;

let content_type_of_body = function
  | String _ -> Some "text/plain"
  | Html _ -> Some "text/html"
  | Json _ -> Some "application/json"
  | Static _ ->
    Some "text/css" (* FIXME: Passar o tipo de acordo com a extensão do arquivo *)
  | EmptyBody -> None
;;

let render response ~view =
  match content_type_of_body view with
  | Some value -> response |> set_header ~key:"Content-Type" ~value |> set_body ~body:view
  | None -> response (* TODO: Setar status 204? *)
;;

let bad_request response = response |> set_status ~status:`BadRequest
