let%expect_test "Basic response" =
  let response = Corcova.Response.empty in
  let string = Corcova.Response.to_string response in
  print_endline string;
  [%expect {|
  HTTP/1.1 200 Ok
  
|}]
;;

let%expect_test "Response with cookies" =
  let response = Corcova.Response.empty in
  let response = Corcova.Response.set_cookie ~key:"name" ~value:"Mateus" response in
  let string = Corcova.Response.to_string response in
  print_endline string;
  [%expect {|
  HTTP/1.1 200 Ok
  Set-Cookie: name=Mateus
|}]
;;
