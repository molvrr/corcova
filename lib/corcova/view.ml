type element =
  | Div of
      { children : element list
      ; attributes : (string * string) list
      }
  | Paragraph of
      { children : element list
      ; attributes : (string * string) list
      }
  | Text of string
  | UnorderedList of
      { children : element list
      ; attributes : (string * string) list
      }
  | Image of
      { alt : string
      ; src : string
      }

type t =
  { title : string
  ; body : element list
  }

let html ~title ~body = { body; title }
let ul attributes children = UnorderedList { children; attributes }
let div attributes children = Div { children; attributes }
let p attributes children = Paragraph { children; attributes }
let txt string = Text string
let img ~alt ~src = Image { alt; src }

let rec render_element html =
  match html with
  | Div { children; _ } ->
    "<div>"
    ^ (List.fold_left (fun acc v -> acc ^ render_element v)) "" children
    ^ "</div>"
  | Paragraph { children; _ } ->
    "<p>" ^ (List.fold_left (fun acc v -> acc ^ render_element v)) "" children ^ "</p>"
  | Text string -> string
  | UnorderedList { children = _children; _ } -> "<ul>" ^ "</ul>"
  | Image { alt; src } -> "<img src=\"" ^ src ^ "\"" ^ " alt=\"" ^ alt ^ "\"/>"
;;

let to_string { body; title } =
  let body = List.map render_element body in
  let rendered_body = String.concat "" body in
  let title = "<title>" ^ title ^ "</title>" in
  let document =
    [ "<!DOCTYPE html><html>"
    ; "<head>"
    ; title
    ; "</head>"
    ; "<body>"
    ; rendered_body
    ; "</body>"
    ]
  in
  String.concat "" document
;;
