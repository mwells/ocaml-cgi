(** Tests for the scgi module *)
open Ocamlcgi_common
open Test_common

let mock_request () =
  let null = Char.chr 0 in
  let b = Buffer.create 200 in
  Buffer.add_string b "70:";
  Buffer.add_string b "CONTENT_LENGTH";
  Buffer.add_char   b null;
  Buffer.add_string b "27";
  Buffer.add_char   b null;
  Buffer.add_string b "SCGI";
  Buffer.add_char   b null;
  Buffer.add_string b "1";
  Buffer.add_char   b null;
  Buffer.add_string b "REQUEST_METHOD";
  Buffer.add_char   b null;
  Buffer.add_string b "POST";
  Buffer.add_char   b null;
  Buffer.add_string b "REQUEST_URI";
  Buffer.add_char   b null;
  Buffer.add_string b "/deepthought";
  Buffer.add_char   b null;
  Buffer.add_string b ",";
  Buffer.add_string b "What is the answer to life?";
  Buffer.contents b

let tests =
  ["scgi_header", (fun () ->
     let result = Scgi.Scgi_headers.of_string @< mock_request () in
     assert_int "same length list" (List.length result) 4
   );
   "scgi_request", (fun () ->
     lwt r = Scgi.Scgi_request.of_stream @< Lwt_stream.of_string @< mock_request () in
     let open Scgi.Scgi_request in
     lwt () = assert_int "content_length" 27 r.content_length in
     lwt () =
       assert_equal
         ~printer:Scgi.Http_method.to_string
         ~msg:"method"
         ~expected:`POST
         r.request_method
     in
     lwt () = assert_string "uri" "/deepthought" r.request_uri in
     lwt () = assert_int ~msg:"remaining headers length" ~expected:0 @< List.length r.request_headers in
     Lazy.force r.content >>= (assert_string ~msg:"content" ~expected:"What is the answer to life?")
   );
  ]

let _ = run tests
