open Corcova.View

(* NOTE: Adicionar um campo com informações extras no request? *)
let index (req : Corcova.Request.t) =
  let username_opt = Corcova.Request.get_cookie ~key:"username" req in
  Layout.make_view
    ~title:"Início"
    [ h1 @@ "Olá, " ^ Option.value ~default:"pessoa" username_opt
    ; form `Post "/logout" [ button "Logout" ]
    ]
;;

let login (_req : Corcova.Request.t) =
  Layout.make_view
    ~title:"Login"
    [ form
        `Post
        "/login"
        [ input ~attributes:[ input_placeholder "Username" ] ~name:"username" `Text
        ; input ~attributes:[ input_placeholder "Password" ] ~name:"password" `Password
        ; input `Submit
        ]
    ; a ~href:"/signup" "Signup"
    ]
;;

let banana =
  Layout.make_view
    ~title:"banana"
    [ img
        []
        ~alt:"Uma banana"
        ~src:
          "https://t4.ftcdn.net/jpg/00/53/03/69/360_F_53036913_Fn9JosHqTi97KQJ40q4i4zgimPhJhLAm.jpg"
    ]
;;
