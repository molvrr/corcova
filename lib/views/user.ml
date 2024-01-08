open Corcova.View
module H = Tyxml.Html

(* NOTE: Adicionar um campo com informações extras no request? *)
let index (req : Corcova.Request.t) =
  let username_opt = Corcova.Request.get_cookie ~key:"username" req in
  Layout.make_view
    ~title:"Início"
    [ H.h1 [ H.txt ("Olá, " ^ Option.value ~default:"pessoa" username_opt) ]
    ; H.form ~a:[ H.a_action "/logout"; H.a_method `Post ] [ H.button [ H.txt "logout" ] ]
    ]
;;

let login (_req : Corcova.Request.t) =
  Layout.make_view
    ~title:"Login"
    [ H.form
        ~a:[ H.a_id "login-form"; H.a_method `Post; H.a_action "/login" ]
        [ H.input ~a:[ H.a_placeholder "Username"; H.a_name "username" ] ()
        ; H.input
            ~a:
              [ H.a_placeholder "Password"
              ; H.a_name "password"
              ; H.a_input_type `Password
              ]
            ()
        ; H.input ~a:[ H.a_input_type `Submit; H.a_value "Login" ] ()
        ]
    ; H.a ~a:[ H.a_href "/signup" ] [ H.txt "Signup" ]
    ]
;;

let banana =
  Layout.make_view
    ~title:"banana"
    [ H.img
        ~alt:"Uma banana"
        ~src:
          "https://t4.ftcdn.net/jpg/00/53/03/69/360_F_53036913_Fn9JosHqTi97KQJ40q4i4zgimPhJhLAm.jpg"
        ()
    ]
;;
