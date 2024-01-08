type route
type t

val empty : t
val set_host : t -> host:string -> t
val set_port : t -> port:int -> t
val run : t -> unit

module Request = Request
module Response = Response
module View = View
module Middleware = Middleware

type handler = Middleware.handler
type middleware = Middleware.t

module Router : sig
  val scope : ?prefix:string -> middleware list -> route list -> route list
  val get : string -> (Request.t -> Response.t -> Response.t) -> route
  val post : string -> (Request.t -> Response.t -> Response.t) -> route
  val router : route list -> middleware list -> t -> t

  module Utils : sig
    val path : route -> string
    val verb : route -> string
  end

  module Debug : sig
    val make : route list -> route list
  end
end
