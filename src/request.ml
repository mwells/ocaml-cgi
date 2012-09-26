(** SCGI request *)
open Lwt
open Batteries

type t = {
  content_length : int;
  meth : Http_method.t;
  uri : Uri.t;
  headers : (string * string) list;
  content : string Lwt.t;
  get_params : (string * string) list;
  post_params : (string * string) list Lwt.t;
}

type header =
  [ `Http_cookie
  | `Http_accept_charset
  | `Http_accept_language
  | `Http_accept_encoding
  | `Http_referer
  | `Http_accept
  | `Http_content_type
  | `Http_user_agent
  | `Http_origin
  | `Http_cache_control
  | `Http_content_length
  | `Http_connection
  | `Http_host
  | `Http_authorization
  | `Server_name
  | `Server_port
  | `Remote_port
  | `Remote_addr
  | `Server_protocol
  | `Other of string
  ]

let header' headers name =
  let name =
    match name with
      | `Http_cookie -> "http_cookie"
      | `Http_accept_charset -> "http_accept_charset"
      | `Http_accept_language -> "http_accept_language"
      | `Http_accept_encoding -> "http_accept_encoding"
      | `Http_referer -> "http_referer"
      | `Http_accept -> "http_accept"
      | `Http_content_type -> "http_content_type"
      | `Http_user_agent -> "http_user_agent"
      | `Http_origin -> "http_origin"
      | `Http_cache_control -> "http_cache_control"
      | `Http_content_length -> "http_content_length"
      | `Http_connection -> "http_connection"
      | `Http_host -> "http_host"
      | `Http_authorization -> "http_authorization"
      | `Server_name -> "server_name"
      | `Server_port -> "server_port"
      | `Remote_port -> "remote_port"
      | `Remote_addr -> "remote_addr"
      | `Server_protocol -> "server_protocol"
      | `Other s -> String.lowercase s
  in
  List.map snd
    (List.find_all (fun (n, _) -> n = name) headers)

let make content_length meth uri headers content =
  let headers = List.map (fun (k, v) -> String.lowercase k, v) headers in
  { content_length;
    meth;
    uri;
    headers = headers;
    content;
    get_params = Uri.query uri;
    post_params =
      match meth with
        | `POST when header' headers `Http_content_type = ["application/x-www-form-urlencoded"] ->
            content >>= fun s ->
            Lwt.return (Uri.query_of_encoded s)
        | _ -> Lwt.return []
  }

let to_string t =
  t.content >>= fun content ->
  t.post_params >>= fun post_params ->
  let s lst = String.concat "; " (List.map (fun (n, v) -> Printf.sprintf "(\"%s\", \"%s\")" n v) lst) in
  Lwt.return
    (Printf.sprintf
       "{ content_length: %d; meth: %s; uri: \"%s\"; headers: [ %s]; content: \"%s\"; get_params: [ %s]; post_params: [ %s] }"
       t.content_length
       (Http_method.to_string t.meth)
       (Uri.to_string t.uri)
       (s t.headers)
       content
       (s t.get_params)
       (s post_params)
    )

let of_stream stream =
  lwt decoded = Netstring.decode stream in
  match Headers.of_string decoded with
    | ("CONTENT_LENGTH", content_length) :: rest ->
      (* CONTENT_LENGTH must be first header according to spec *)
      lwt content_length =
        try_lwt Lwt.return (int_of_string content_length)
        with _ ->  raise_lwt (Failure ("Invalid content_length: [" ^ content_length ^ "]"))
      in
      Lwt.wrap (fun () ->
        (* Process the remaining headers *)
        let (scgi, request_method, uri, headers) =
          List.fold_left
            (fun  (s, m, u, h) -> function
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
            make
              content_length
              (Http_method.of_string request_method )
              (Uri.of_string uri)
              headers
              (Lwt_stream.nget content_length stream >>= fun chars ->
               let b = Buffer.create content_length in
               List.iter (Buffer.add_char b) chars;
               Lwt.return (Buffer.contents b)
              )
          | ""   -> raise (Failure "Missing SCGI header")
          | _    -> raise (Failure "Unexpected SCGI header")
        )  (* wrap *)
    | (n, _) :: _ -> raise_lwt (Failure ("Expected CONTENT_LENGTH, but got [" ^ n ^ "]"))
    | []          -> raise_lwt (Failure "No headers found")

let content_length t = t.content_length
let meth t = t.meth
let uri t = t.uri
let path t = Uri.path t.uri
let contents t = t.content
let param ?meth ?default t name =
  Lwt.catch
    (fun () ->
      let rec loop = function
        | None ->
            Lwt.catch
              (fun () -> loop (Some `POST))
              (function
                | Not_found -> loop (Some `GET)
                | e -> Lwt.fail e)
        | Some `GET ->
            Lwt.wrap (fun () -> List.assoc name t.get_params)
        | Some `POST ->
            t.post_params >>= fun post_params ->
            Lwt.wrap (fun () -> List.assoc name post_params)
      in
      loop meth
    )
    (function
      | Not_found ->
          begin match default with
            | Some s -> Lwt.return s
            | None   -> Lwt.fail Not_found
          end
      | e -> Lwt.fail e
    )

let params_get t = t.get_params

let header t name = header' t.headers name

let cookie t name =
  match header' t.headers `Http_cookie with
    | [] -> None
    | cookies :: _ ->
        try
          Some (String.nsplit ~by:"; " cookies
                   |> List.map (String.split ~by:"=")
                   |> List.assoc name)
        with Not_found -> None
