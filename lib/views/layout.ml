module V = Corcova.View

let make_view ~title element =
  V.html ~title ~body:[ V.div [] [ V.txt "Corcova"; element ] ]
;;
