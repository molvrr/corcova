module H = Tyxml.Html

let make_view ~title elements =
  let title = H.title (H.txt title) in
  let head =
    H.head
      title
      [ H.meta ~a:[ H.a_charset "UTF-8" ] ()
      ; H.link ~rel:[ `Stylesheet ] ~href:"style.css" ()
      ]
  in
  let body = H.body elements in
  Corcova.Response.Html (H.html head body)
;;
