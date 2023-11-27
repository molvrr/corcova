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
  let response = response |> render_html ~view:(html ~title:"Início" ~body) in
  let string = Corcova.Response.to_string response in
  print_endline string;
  [%expect
    {|
    HTTP/1.1 200 Ok
    Content-Type: text/html
    Content-Length: 151
    
    <!DOCTYPE html><html><head><title>Início</title></head><body><div><p><p/> inside a <div/></p><div><p><p/> inside another <div/></p></div></div></body> |}]
;;

let%expect_test "Infix with a path which is a slash and another with a leading slash" =
  let open Corcova.Router.Infix in
  let p1 = "/" in
  let p2 = "/api" in
  print_endline (p1 / p2);
  [%expect {|/api|}]
;;

let%expect_test "Infix with a path which has a trailing slash and another with a leading \
                 slash"
  =
  let open Corcova.Router.Infix in
  let p1 = "api/" in
  let p2 = "/users" in
  print_endline (p1 / p2);
  [%expect {|/api/users|}]
;;
