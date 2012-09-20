type server_name = string
type inet_addr = string
type port = int

let default_read_error_handler exn =
  prerr_endline (Printexc.to_string exn ^ "\n" ^ Printexc.get_backtrace ());
  Lwt.return
    { Response.status = `Internal_server_error;
      headers = [`Content_type "text/plain"];
      body = `String (Printexc.to_string exn);
    }

let default_write_error_handler exn =
  prerr_endline (Printexc.to_string exn ^ "\n" ^ Printexc.get_backtrace ());
  Lwt.return ()

let handler
    ?(read_error_handler=default_read_error_handler)
    ?(write_error_handler=default_write_error_handler)
    name
    inet_addr
    port
    f =
  let _server =
    Lwt_io.establish_server
      (Unix.ADDR_INET (Unix.inet_addr_of_string inet_addr, port))
      (fun (inch, ouch) ->
        Lwt.ignore_result (
          lwt response =
            try_lwt
              lwt request = Request.of_stream (Lwt_io.read_chars inch) in
              f request
            with e ->
              read_error_handler e
          in
          try_lwt
            let open Response in
            (* Add content length from body if not already in the headers *)
            let is_content_length_in_headers =
              List.exists
                (function `Content_length _ -> true | _ -> false)
                response.headers
            in
            let response_headers =
              if is_content_length_in_headers then
                response.headers
              else
                match response.body with
                  | `Stream (Some l, _) -> `Content_length l :: response.headers
                  | `String s           -> `Content_length (String.length s) :: response.headers
                  | `Stream (None, _)   -> response.headers
            in

            (* Write headers *)
            lwt () =
              Lwt_util.iter
                (fun h -> Lwt_io.write ouch (Http_header.to_string h))
                (`Status response.status :: response_headers)
            in

            (* Blank line between headers and body *)
            lwt () = Lwt_io.write ouch "\r\n" in

            (* Write the body *)
            lwt () =
              match response.body with
                | `Stream (_, s) -> Lwt_io.write_chars ouch s
                | `String s      -> Lwt_io.write ouch s
            in

            (* The server closes the connection *)
            Lwt_io.close ouch
          with e -> write_error_handler e
        )  (* Lwt.ignore_result *)
      )  (* fun (inch, ouch) -> *)
  in
  print_endline (Printf.sprintf "Started [%s] listening on %s:%d" name inet_addr port)
