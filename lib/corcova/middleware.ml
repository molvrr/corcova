type handler = Request.t -> Response.t -> Request.t * Response.t
type t = handler -> handler

let no_cache : t =
  fun handler req res ->
  handler req (Response.set_header ~key:"Cache-Control" ~value:"no-store" res)
;;

let json : t =
  let open Request in
  fun handler req ->
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

let logger : t =
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
