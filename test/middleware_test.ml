let () =
  let open Alcotest in
  run "middleware"
    [
      ( "Basic functionality",
        [
          ( test_case "Accesses the input and output of a wrapped function call"
              `Quick
          @@ fun () ->
            (****** Arrange ******)
            let add_three (x : int) : int = x + 3 in
            let multiply_by_five_then_stringify :
                (int, int, int, string) Middleware.t =
             fun (input : int) : (int, int, string) Middleware.Diverter.t ->
              Middleware.continue (input * 5) string_of_int
            in
            let composed =
              Middleware.Infix.(multiply_by_five_then_stringify <&> add_three)
            in

            (****** Act ******)
            let actual = composed 1234 in

            (****** Assert ******)
            check string "equal" actual (string_of_int ((1234 * 5) + 3)) );
          (*
           *
           *)
          ( test_case
              "Stops, preventing the delegate function from being called" `Quick
          @@ fun () ->
            (****** Arrange ******)
            let panic (_ : int) : int = failwith "Time to panic!" in
            let dont_panic : (int, int, int, string) Middleware.t =
             fun (input : int) : (int, int, string) Middleware.Diverter.t ->
              Middleware.stop (string_of_int input)
            in
            let composed = Middleware.Infix.(dont_panic <&> panic) in

            (****** Act ******)
            let actual = composed 1234 in

            (****** Assert ******)
            check string "equal" actual "1234" );
          (*
           *
           *)
          ( test_case "Multiple middleware can be composed" `Quick @@ fun () ->
            (****** Arrange ******)
            let add_three (x : int) : int = x + 3 in
            let multiply_by_five_then_stringify :
                (int, int, int, string) Middleware.t =
             fun (input : int) : (int, int, string) Middleware.Diverter.t ->
              Middleware.continue (input * 5) string_of_int
            in
            let truncate_then_append_foo :
                (float, int, string, string) Middleware.t =
             fun (input : float) : (int, string, string) Middleware.Diverter.t ->
              Middleware.continue (int_of_float input) (fun s -> s ^ "_foo")
            in
            let composed =
              Middleware.Infix.(
                truncate_then_append_foo <<>> multiply_by_five_then_stringify
                <&> add_three)
            in

            (****** Act ******)
            let actual = composed 1234.9999 in

            (****** Assert ******)
            check string "equal" actual "6173_foo" );
        ] );
      ( "Monadic behavior",
        [
          ( test_case "Works in a monadic context" `Quick @@ fun () ->
            (****** Arrange ******)
            let module Middleware = Middleware.Make (struct
              include Option

              let return = Option.some
            end) in
            let add_three (x : int) : int option = Some (x + 3) in
            let multiply_by_five_then_stringify :
                (int, int, int, string) Middleware.t =
             fun (input : int) : (int, int, string) Middleware.Diverter.t ->
              Middleware.continue (input * 5) (fun x -> Some (string_of_int x))
            in
            let composed =
              Middleware.Infix.(multiply_by_five_then_stringify <&> add_three)
            in

            (****** Act ******)
            let actual = composed 1234 in

            (****** Assert ******)
            check (option string) "equal" actual (Some "6173") );
          (*
           *
           *)
          ( test_case "Obeys monadic context for terminal functions" `Quick
          @@ fun () ->
            (****** Arrange ******)
            let module Middleware = Middleware.Make (struct
              include Option

              let return = Option.some
            end) in
            let return_none (_ : int) : int option = None in
            let multiply_by_five_then_stringify :
                (int, int, int, string) Middleware.t =
             fun (input : int) : (int, int, string) Middleware.Diverter.t ->
              Middleware.continue (input * 5) (fun x -> Some (string_of_int x))
            in
            let composed =
              Middleware.Infix.(multiply_by_five_then_stringify <&> return_none)
            in

            (****** Act ******)
            let actual = composed 1234 in

            (****** Assert ******)
            check (option string) "equal" actual None );
          (*
           *
           *)
          ( test_case "Obeys monadic context for middleware functions" `Quick
          @@ fun () ->
            (****** Arrange ******)
            let module Middleware = Middleware.Make (struct
              include Option

              let return = Option.some
            end) in
            let add_three (x : int) : int option = Some (x + 3) in
            let multiply_by_five_then_return_none :
                (int, int, int, string) Middleware.t =
             fun (input : int) : (int, int, string) Middleware.Diverter.t ->
              Middleware.continue (input * 5) (fun _ -> None)
            in
            let composed =
              Middleware.Infix.(multiply_by_five_then_return_none <&> add_three)
            in

            (****** Act ******)
            let actual = composed 1234 in

            (****** Assert ******)
            check (option string) "equal" actual None );
          (*
           *
           *)
          ( test_case "Supports two-parameter monads" `Quick @@ fun () ->
            (****** Arrange ******)
            let module Middleware = Middleware.Make2 (struct
              include Result

              let return = Result.ok
            end) in
            let add_three (x : int) : (int, string) result = Ok (x + 3) in
            let multiply_by_five_then_return_error :
                (int, int, int, string, string) Middleware.t =
             fun (input : int) :
                 (int, int, string, string) Middleware.Diverter.t ->
              Middleware.continue (input * 5) (fun _ -> Error "oopsies")
            in
            let composed =
              Middleware.Infix.(
                multiply_by_five_then_return_error <&> add_three)
            in

            (****** Act ******)
            let actual = composed 1234 in

            (****** Assert ******)
            check (result string string) "equal" actual (Error "oopsies") );
        ] );
    ]
