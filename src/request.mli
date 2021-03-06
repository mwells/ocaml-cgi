(** SCGI request *)
type t

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
  | `Server_name
  | `Server_port
  | `Remote_port
  | `Remote_addr
  | `Server_protocol
  | `Other of string
  ]

val make : int -> Http_method.t -> Uri.t -> Headers.t -> string Lwt.t -> t
val of_stream : char Lwt_stream.t -> t Lwt.t

val content_length : t -> int
val meth : t -> Http_method.t
val uri : t -> Uri.t
val path : t -> string
val contents : t -> string Lwt.t
val param : ?meth:[ `GET | `POST ] -> ?default:string -> t  -> string -> string Lwt.t
val params_get : t -> (string * string) list
val header : t -> header -> string list
val cookie : t -> string -> string option

val to_string : t -> string Lwt.t
  (** For debugging *)
