(** HTTP request method *)
type t = [`DELETE | `GET | `HEAD | `POST | `PUT]

let of_string = function
  | "DELETE" -> `DELETE
  | "GET"    -> `GET
  | "HEAD"   -> `HEAD
  | "POST"   -> `POST
  | "PUT"    -> `PUT
  | s        -> failwith ("Invalid request method: " ^ s)

let to_string = function
  | `DELETE  -> "DELETE"
  | `GET     -> "GET"
  | `HEAD    -> "HEAD"
  | `POST    -> "POST"
  | `PUT     -> "PUT"
