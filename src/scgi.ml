(** Scgi implementation *)

open Ocamlcgi_common

(** HTTP request method *)
module Http_method =
struct
  type t = [`DELETE | `GET | `HEAD | `POST | `PUT]

  let of_string = function
    | "DELETE" -> `DELETE
    | "GET"    -> `GET
    | "HEAD"   -> `HEAD
    | "POST"   -> `POST
    | "PUT"    -> `PUT
    | s        -> failwith @< "Invalid request method: " ^ s

  let to_string = function
    | `DELETE  -> "DELETE"
    | `GET     -> "GET"
    | `HEAD    -> "HEAD"
    | `POST    -> "POST"
    | `PUT     -> "PUT"
end

(** Non-comprehensive list of HTTP response status codes and strings from
    http://en.wikipedia.org/wiki/List_of_HTTP_status_codes *)
module Http_status =
struct
  type t =
      [ `Ok
      | `Created
      | `Accepted
      | `Non_authoritative_information
      | `No_content
      | `Reset_content
      | `Partial_content
      | `Multiple_choices
      | `Moved_permanently
      | `Found
      | `See_other
      | `Not_modified
      | `Temporary_redirect
      | `Bad_request
      | `Unauthorized
      | `Payment_required
      | `Forbidden
      | `Not_found
      | `Method_not_allowed
      | `Not_acceptable
      | `Proxy_authentication_required
      | `Request_timeout
      | `Conflict
      | `Gone
      | `Length_required
      | `Precondition_failed
      | `Request_entity_too_large
      | `Request_uri_too_long
      | `Unsupported_media_type
      | `Requested_range_not_satisfiable
      | `Expectation_failed
      | `Internal_server_error
      | `Not_implemented
      | `Bad_gateway
      | `Service_unavailable
      | `Gateway_timeout
      | `Http_version_not_supported
      ]

  let values = function
    | `Ok                              -> 200, "OK"
    | `Created                         -> 201, "Created"
    | `Accepted                        -> 202, "Accepted"
    | `Non_authoritative_information   -> 203, "Non-Authoritative Information"
    | `No_content                      -> 204, "No Content"
    | `Reset_content                   -> 205, "Reset Content"
    | `Partial_content                 -> 206, "Partial Content"
    | `Multiple_choices                -> 300, "Multiple Choices"
    | `Moved_permanently               -> 301, "Moved Permanently"
    | `Found                           -> 302, "Found"
    | `See_other                       -> 303, "See Other"
    | `Not_modified                    -> 304, "Not Modified"
    | `Temporary_redirect              -> 307, "Temporary Redirect"
    | `Bad_request                     -> 400, "Bad Request"
    | `Unauthorized                    -> 401, "Unauthorized"
    | `Payment_required                -> 402, "Payment Required"
    | `Forbidden                       -> 403, "Forbidden"
    | `Not_found                       -> 404, "Not Found"
    | `Method_not_allowed              -> 405, "Method Not Allowed"
    | `Not_acceptable                  -> 406, "Not Acceptable"
    | `Proxy_authentication_required   -> 407, "Proxy Authentication Required"
    | `Request_timeout                 -> 408, "Request Timeout"
    | `Conflict                        -> 409, "Conflict"
    | `Gone                            -> 410, "Gone"
    | `Length_required                 -> 411, "Length Required"
    | `Precondition_failed             -> 412, "Precondition Failed"
    | `Request_entity_too_large        -> 413, "Request Entity Too Large"
    | `Request_uri_too_long            -> 414, "Request URI Too Long"
    | `Unsupported_media_type          -> 415, "Unsupported Media Type"
    | `Requested_range_not_satisfiable -> 416, "Request Range Not Satisfiable"
    | `Expectation_failed              -> 417, "Expectation Failed"
    | `Internal_server_error           -> 500, "Internal Server Error"
    | `Not_implemented                 -> 501, "Not Implemented"
    | `Bad_gateway                     -> 502, "Bad Gateway"
    | `Service_unavailable             -> 503, "Service Unavailable"
    | `Gateway_timeout                 -> 504, "Gateway Timeout"
    | `Http_version_not_supported      -> 505, "HTTP Version Not Supported"

  let to_int    = fst @<< values
  let to_string = snd @<< values
end

(** SCGI request headers *)
module Scgi_headers =
struct
  type t = (string * string) list

  let null = Char.chr 0

  let of_string s =
    let rec loop lst index =
      try
        let next = String.index_from s index null in
        let name = String.sub s index (next - index) in
        let index = next + 1 in
        let next = String.index_from s index null in
        let value = String.sub s index (next - index) in
        loop ((name, value) :: lst) (next + 1)
      with Not_found -> List.rev lst
    in
    loop [] 0
end

(** SCGI request *)
module Scgi_request =
struct
  type t = {
    content_length : int;
    request_method : Http_method.t;
    request_uri : string;
    request_headers : (string * string) list;
    content : string Lwt.t Lazy.t;
  }

  let make content_length request_method request_uri request_headers content =
    { content_length;
      request_method;
      request_uri;
      request_headers;
      content;
    }

  let of_stream stream =
    Netstring.decode stream >>= fun headers -> Scgi_headers.of_string headers |> function
      | ("CONTENT_LENGTH", content_length) :: rest ->
          (* CONTENT_LENGTH must be first header according to spec *)
          Lwt.catch
            (fun () -> Lwt.return @< int_of_string content_length)
            (fun _  -> raise_lwt (Failure ("Invalid content_length: [" ^ content_length ^ "]")))
          >>= (safely @< fun content_length ->
            (* Process the remaining headers *)
            let (scgi, request_method, uri, headers) =
              List.fold_left (fun  (s, m, u, h) -> function
                  (* Look for known headers first *)
                | ("SCGI",           s) -> (s, m, u, h)
                | ("REQUEST_METHOD", m) -> (s, m, u, h)
                | ("REQUEST_URI",    u) -> (s, m, u, h)

                  (* Accumulate unknown headers *)
                | header                -> (s, m, u, header :: h)
              )
              ("", "", "", [])
              rest
            in
            match scgi with
              | "1"  ->
                  (* SCGI header must be 1 according to spec *)
                  Lwt.return @< make
                    content_length
                    (Http_method.of_string request_method )
                    uri
                    headers
                    (lazy (Lwt_stream.nget content_length stream >>= fun chars ->
                             let b = Buffer.create content_length in
                             List.iter (Buffer.add_char b) chars;
                             Lwt.return @< Buffer.contents b
                          )
                    )
                | ""   -> raise_lwt (Failure "Missing SCGI header")
                | _    -> raise_lwt (Failure "Unexpected SCGI header")
          )
      | (n, _) :: _ -> raise_lwt (Failure ("Expected CONTENT_LENGTH, but got [" ^ n ^ "]"))
      | []          -> raise_lwt (Failure "No headers found")
end

(** SCGI response *)
module Scgi_response =
struct
  type t = {
    status  : Http_status.t;
    headers : (string * string) list;
    body    : char Lwt_stream.t
  }
end

let handler name inet_addr port f =
  let _server =
    Lwt_io.establish_server
      (Unix.ADDR_INET (Unix.inet_addr_of_string inet_addr, port))
      (fun (inch, ouch) ->
        Lwt.ignore_result
          (Lwt.catch
              (fun () -> Scgi_request.of_stream (Lwt_io.read_chars inch) >>= f)
              (fun e ->
                (* TODO log it *)
                print_endline (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ());
                Lwt.return
                  { Scgi_response.status = `Internal_server_error;
                    headers = ["Content-Type", "text/plain"];
                    body = Lwt_stream.of_string @< Printexc.to_string e;
                  }
              ) >>= fun response ->
           Lwt.catch
             (fun () ->
               let open Scgi_response in
               let status_header =
                 "Status",
                 Printf.sprintf "%d %s"
                   (Http_status.to_int response.status)
                   (Http_status.to_string response.status)
               in

               List.iter
                 (fun (name, value) ->
                   Lwt.ignore_result (Lwt_io.write ouch (Printf.sprintf "%s: %s\r\n" name value))
                 )
                 (status_header :: response.headers);

               (* Blank line between headers and body *)
               Lwt_io.write ouch "\r\n" >>= fun () ->
               Lwt_io.write_chars ouch response.body
             )
             (fun e -> (* TODO log it *) Lwt.return ())
           >>= (fun () ->
             (* The server closes the connection *)
             Lwt_io.close ouch)
          )
      )
  in
  print_endline @< Printf.sprintf "Started [%s] listening on %s:%d" name inet_addr port
