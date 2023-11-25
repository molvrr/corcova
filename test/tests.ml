open Corcova.Response

let%expect_test "Basic response" =
  let string = empty |> to_string in
  print_endline string;
  [%expect {|
  HTTP/1.1 200 Ok
  
|}]
;;

let%expect_test "Response with cookies" =
  let response = empty |> set_cookie ~key:"name" ~value:"banana" in
  let string = to_string response in
  print_endline string;
  [%expect {|
  HTTP/1.1 200 Ok
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
  Set-Cookie: name=Mateus
  Content-Length: 11
  
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
