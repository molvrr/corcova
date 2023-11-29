open Corcova.Response

let%expect_test "Basic response" =
  let string = empty |> to_string in
  print_endline string;
  [%expect {|
  HTTP/1.1 200 Ok
  
|}]
;;

let%expect_test "Response with cookies" =
  let response =
    empty
    |> set_cookie ~key:"name" ~value:"banana"
    |> set_cookie ~key:"name2" ~value:"banana2"
  in
  let string = to_string response in
  print_endline string;
  [%expect
    {|
  HTTP/1.1 200 Ok
  Set-Cookie: name2=banana2
  Set-Cookie: name=banana
  
|}]
;;

let%expect_test "Response with body" =
  let response =
    empty
    |> set_cookie ~key:"name" ~value:"Mateus"
    |> set_body ~body:(String "<h1>Oi</h1>")
  in
  let string = to_string response in
  print_endline string;
  [%expect
    {|
  HTTP/1.1 200 Ok
  Content-Length: 11
  Set-Cookie: name=Mateus
  
  <h1>Oi</h1>
|}]
;;

let%expect_test "Redirect" =
  let response = empty |> redirect ~path:"/login" in
  let string = to_string response in
  print_endline string;
  [%expect {|
    HTTP/1.1 301 Moved Permanently
    Location: /login
     |}]
;;

let%expect_test "View render" =
  let open Corcova.View in
  let response = empty in
  let body =
    [ div
        []
        [ p [] [ txt "<p/> inside a <div/>" ]
        ; div [] [ p [] [ txt "<p/> inside another <div/>" ] ]
        ]
    ]
  in
  let response = response |> render ~view:(Html (html "Início" body)) in
  let string = Corcova.Response.to_string response in
  print_endline string;
  [%expect
    {|
    HTTP/1.1 200 Ok
    Content-Type: text/html
    Content-Length: 151
    
    <!DOCTYPE html><html><head><title>Início</title></head><body><div><p><p/> inside a <div/></p><div><p><p/> inside another <div/></p></div></div></body> |}]
;;
