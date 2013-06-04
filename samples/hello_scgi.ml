(** A simple hello world scgi
 *
 *  E.g. ./_build/hello_scgi.byte
 *)

open Scgi
open Lwt

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
    (fun s -> failwith (Printf.sprintf "Unknown argument: [%s]" s))
    "try --help";

  (* Start the handler *)
  Server.handler_inet "hello" !addr !port (fun r ->
    Lwt.return
      { Response.status = `Ok;
        headers = [`Content_type "text/plain"];
        body = `String ("Hello world: " ^ Request.path r);
      }
  );

  (* Run forever in foreground. *)
  Lwt_main.run (fst (Lwt.wait ()))










