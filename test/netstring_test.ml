(** Tests for the netstring module *)
open Ocamlcgi_common
open Test_common

let tests =
  ["decode", fun () ->
    let stream = Lwt_stream.of_string "12:hello world!," in
    Netstring.decode stream >>= fun s ->
    assert_string "12:hello world!" "hello world!" s;
  ]

let _ = run tests
