(** The SCGI server *)
type server_name = string
type inet_addr = string
type port = int

val handler :
  ?read_error_handler:(exn -> Response.t Lwt.t) ->
  ?write_error_handler:(exn -> unit Lwt.t) ->
  server_name ->
  inet_addr ->
  port ->
  (Request.t -> Response.t Lwt.t) ->
  unit
