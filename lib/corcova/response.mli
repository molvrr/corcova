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
  | Html of Tyxml.Html.doc
  | Json of Yojson.Safe.t
  | Static of string

type t

val empty : t
val body_to_string : t -> string
val body_of_response : t -> body
val set_header : t -> key:string -> value:string -> t
val set_body : t -> body:body -> t
val set_status : t -> status:http_status -> t

val set_cookie
  :  ?attributes:(string * string) list
  -> t
  -> key:string
  -> value:string
  -> t

val status : t -> http_status
val redirect : t -> path:string -> t
val headers_to_string : t -> string
val status_to_http : t -> string
val to_string : t -> string
val render : t -> view:body -> t
val bad_request : t -> t
