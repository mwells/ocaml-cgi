let (>>=) = Lwt.(>>=)
  (** bind *)

let (|>) x f = f x
  (** Pipeline in F# syntax *)

let (|>>) f g x = f x |> g
  (** Reverse compose: F#'s >> *)

let (@<) f x = f x
  (** Reverse pipeline (right associative): F#'s <| *)

let (@<<) f g x = f (g x)
  (** Compose: Jane Street's $, Haskell's ., F#'s ... *)

let safely f x =
  try f x
  with e -> raise_lwt e

