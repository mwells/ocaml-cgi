(** SCGI response *)
type body =
  [ `Stream of int option * char Lwt_stream.t  (* content-length, stream *)
  | `String of string  (* content-length added automatically *)
  ]

type t = {
  status  : Http_status.t;
  headers : Http_header.t list;
  body    : body
}

val make : status:Http_status.t -> ?headers:(Http_header.t list) -> ?body:body -> unit -> t

val to_string : ?body_max:int -> t -> string
  (** For debugging *)
