type handler
type middleware
type route
type t

val empty : t
val set_host : t -> host:string -> t
val set_port : t -> port:int -> t
val json : middleware
val logger : middleware
val run : t -> unit

module Response = Response
module View = View

module Router : sig
  val scope : string -> middleware list -> route list -> route list
  val get : string -> (Request.t -> Response.t -> Response.t) -> route
  val post : string -> (Request.t -> Response.t -> Response.t) -> route
  val add_routes : t -> routes:route list -> t
end
