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

let make ~status ?(headers=[]) ?(body=`String "") () =
  { status;
    headers;
    body;
  }
