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

(** HTTP headers *)
module Http_header =
struct
  type t =
      [ `Content_length of int
      | `Content_type of string
      | `Status of Http_status.t
      | `Other of string * string
      ]

  let to_string = function
      | `Content_length l -> Printf.sprintf "Content-Length: %d\r\n" l
      | `Content_type   s -> Printf.sprintf "Content-Type: %s\r\n" s
      | `Status         s -> Printf.sprintf "Status: %d %s\r\n" (Http_status.to_int s) (Http_status.to_string s)
      | `Other      (n,v) -> n ^ ":" ^ v  (* Ought to define more *)
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
  type body =
      [ `Stream of int option * char Lwt_stream.t  (* content-length, stream *)
      | `String of string  (* content-length added automatically *)
      ]

  type t = {
    status  : Http_status.t;
    headers : Http_header.t list;
    body    : body
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
                    headers = [`Content_type "text/plain"];
                    body = `String (Printexc.to_string e);
                  }
              ) >>= fun response ->
           Lwt.catch
             (fun () ->
               let open Scgi_response in

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
               Lwt_util.iter
                 (Lwt_io.write ouch @<< Http_header.to_string)
                 (`Status response.status :: response_headers) >>= fun () ->

               (* Blank line between headers and body *)
               Lwt_io.write ouch "\r\n" >>= fun () ->

               (* Write the body *)
               match response.body with
                 | `Stream (_, s) -> Lwt_io.write_chars ouch s
                 | `String s      -> Lwt_io.write ouch s
             )
             (fun e -> (* TODO log it *) Lwt.return ())
           >>= (fun () ->
             (* The server closes the connection *)
             Lwt_io.close ouch)
          )
      )
  in
  print_endline @< Printf.sprintf "Started [%s] listening on %s:%d" name inet_addr port
