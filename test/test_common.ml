(** Common functions for running tests *)
open Ocamlcgi_common

let assert_equal ~printer ~msg ~expected actual =
  if expected = actual then
    Lwt.return ()
  else
    Lwt.fail
    @< Failure (Printf.sprintf "%s expected [%s] but got [%s]"
                   msg
                   (printer expected)
                   (printer actual)
               )

let assert_string ~msg ~expected actual = assert_equal (fun s -> s) msg expected actual
let assert_int  ~msg ~expected actual = assert_equal string_of_int msg expected actual

let test_runner tests =
  let progress () =
    output_char stderr '.';
    flush stderr
  in
  Lwt.join @< List.map
    (fun (name, fn) ->
      progress ();
      Lwt.catch fn (fun e ->
        let msg =
          Printf.sprintf "\n\nIn test [%s]:\n%s\n%s"
            name
            (Printexc.to_string e)
            (Printexc.get_backtrace ())
        in
        failwith msg
      )
    )
    tests

let run tests =
  Printexc.record_backtrace true;
  Lwt.ignore_result @< test_runner tests;
  print_endline "\nSUCCESS"
