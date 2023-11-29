type attribute = string * string
type href = string
type label = string

type form_data =
  { verb : [ `Post ]
  ; action : string
  ; children : element list
  }

and element =
  [ `Ul of attribute list * element list
  | `Div of attribute list * element list
  | `Paragraph of attribute list * element list
  | `Image of attribute list
  | `Text of string
  | `H1 of string
  | `Form of form_data
  | `Button of attribute list * element list
  | `Input of [ `Text | `Password | `Submit ] * attribute list
  | `Anchor of href * label * attribute list
  ]

type t =
  { title : string
  ; body : element list
  }

let html title body = { title; body }
let ul attributes children = `Ul (attributes, children)
let div attributes children = `Div (attributes, children)
let p attributes children = `Paragraph (attributes, children)

let img (attributes : attribute list) ~(alt : string) ~(src : string) =
  let alt_attr = "alt", alt in
  let src_attr = "src", src in
  `Image (src_attr :: alt_attr :: attributes)
;;

let form_method_to_string = function
  | `Post -> "post"
;;

let a ?(attributes = []) ~href label = `Anchor (href, label, attributes)
let txt string = `Text string
let button ?(attributes = []) label = `Button (attributes, [ txt label ])
let h1 string = `H1 string
let form (verb : [ `Post ]) action children = `Form { verb; action; children }

let input ?(attributes = []) ?name input_type =
  match name with
  | Some name -> `Input (input_type, ("name", name) :: attributes)
  | None -> `Input (input_type, attributes)
;;

let input_value string = "value", string
let input_placeholder string = "placeholder", string

let input_type_to_string = function
  | `Text -> "text"
  | `Password -> "password"
  | `Submit -> "submit"
;;

let rec render_tag tag_name attrs children =
  let attrs = List.map (fun (k, v) -> k ^ "=" ^ v) attrs in
  let attrs = String.concat " " attrs in
  Format.sprintf
    "<%s %s>%s</%s>"
    tag_name
    attrs
    (String.concat "" @@ List.map render_element children)
    tag_name

and render_element = function
  | `Ul (attributes, children) -> render_tag "ul" attributes children
  | `Div (attributes, children) -> render_tag "div" attributes children
  | `Paragraph (attributes, children) -> render_tag "p" attributes children
  | `Image attributes -> render_tag "img" attributes []
  | `Text string -> string
  | `H1 string -> render_tag "h1" [] [ `Text string ]
  | `Form { verb; action; children } ->
    render_tag "form" [ "action", action; "method", form_method_to_string verb ] children
  | `Button (attributes, children) -> render_tag "button" attributes children
  | `Input (input_type, attributes) ->
    render_tag "input" (("type", input_type_to_string input_type) :: attributes) []
  | `Anchor (href, label, attributes) ->
    render_tag "a" (("href", href) :: attributes) [ txt label ]
;;

let to_string { title; body } =
  Format.asprintf
    {html|
     <!DOCTYPE html>
     <html>
        <head>
        <meta charset="UTF-8">
        <title>%s</title>
        </head>
        <body>
     %s
        </body>
    </html>
     |html}
    title
  @@ String.concat "" (List.map render_element body)
;;
