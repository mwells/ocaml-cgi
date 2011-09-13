(** A simple hello world scgi
 *
 *  E.g. ./_build/hello_scgi.byte
 *)

open Ocamlcgi_common

let _ =
  (* Command line options *)
  let port = ref 8080 in
  let addr = ref "127.0.0.1" in

  Arg.parse
    (Arg.align
        ["--port", Arg.Set_int    port, Printf.sprintf " port number (default: %d)" !port;
         "--addr", Arg.Set_string addr, Printf.sprintf " ip address to bind (default: %s)" !addr;
        ]
    )
    (failwith @<< Printf.sprintf "Unknown argument: [%s]")
    "try --help";

  (* Start the handler *)
  Scgi.handler "hello" !addr !port (fun _request ->
    Lwt.return
      { Scgi.Scgi_response.status = `Ok;
        headers = [`Content_type "text/plain"];
        body = `String "Hello world"
      }
  );

  (* Run forever in foreground. *)
  Lwt_main.run (fst @< Lwt.wait ())
