type cookies
type headers

type param =
  | NoParams
  | Json of Yojson.Safe.t

type t =
  { path : string
  ; verb : [ `DELETE | `GET | `PATCH | `POST | `PUT ]
  ; headers : headers
  ; body : Bytes.t option
  ; params : param
  ; cookies : cookies option
  }

val of_fd : Unix.file_descr -> t (* TODO: Ocultar isso *)
val get_cookie : t -> key:string -> string option
val get_header : t -> string -> string option
