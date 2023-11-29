type t
type element
type attribute

val html : string -> element list -> t
val ul : attribute list -> element list -> element
val h1 : string -> element
val div : attribute list -> element list -> element
val p : attribute list -> element list -> element
val img : attribute list -> alt:string -> src:string -> element
val txt : string -> element
val form : [ `Post ] -> string -> element list -> element
val input : ?attributes:attribute list -> ?name:string -> [ `Text | `Password | `Submit ] -> element
val input_value : string -> attribute
val input_placeholder : string -> attribute
val a : ?attributes:attribute list -> href:string -> string -> element
val button : ?attributes:attribute list -> string -> element
val render_element : element -> string
val to_string : t -> string
