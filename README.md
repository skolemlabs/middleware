# middleware

## What does this library do?

`middleware` is a small OCaml library for writing functions which run other
functions inside of them. Here's a short example:

```ocaml
(* Standard OCaml function which adds 3 to any [int]. *)
let add_three (x : int) : int = x + 3 in

(*
  This function is middleware. It:

  - Accepts an [int] argument
  - Delegates to some [int -> int] function [g]
  - Transforms the result of [g] (in this case, to a [string])
 *)
let multiply_by_five_then_stringify :
    (int, int, int, string) Middleware.t =
 fun (input : int) : (int, int, string) Middleware.Diverter.t ->
  Middleware.continue (input * 5) string_of_int
in

(* This function is also middleware. *)
let truncate_then_append_foo :
    (float, int, string, string) Middleware.t =
 fun (input : float) : (int, string, string) Middleware.Diverter.t ->
  Middleware.continue (int_of_float input) (fun s -> s ^ "_foo")
in

(*
  Middleware can be composed, forming a chain of wrapped function calls.

  Here's how this function calls its components:

  - [truncate_then_append_foo] truncates the input to an [int]
  - [multiply_by_five_then_stringify] multiplies it by 5
  - [add_three] adds 3 to it
  - [multiply_by_five_then_stringify] stringifies it
  - [truncate_then_append_foo] append "_foo" to the result
*)
let composed: float -> string =
  Middleware.Infix.(
    truncate_then_append_foo
    <<>> multiply_by_five_then_stringify
    <&> add_three)
in

composed 1234.999  (* = "6173_foo" *)
```

## Why is this library useful?

This is mostly useful for setting up, then tearing down some context for an
inner function. For example, we might want to write a middleware which can
time a function call:

```ocaml
let time_milliseconds : ('a, 'a, 'b, 'b * float) Middleware.t =
 fun (a : 'a) : ('a, 'b, 'b * float) Middleware.Diverter.t ->
  let start_time = Unix.gettimeofday () in
  Middleware.continue a (fun b ->
      let end_time = Unix.gettimeofday () in
      (b, end_time -. start_time))
```

Or, perhaps you'd like to authenticate a request for any function which requires
one:

```ocaml
let run_authenticated :
    (Request.t, Authenticated_request.t, 'b, 'b) Middleware.t =
 fun (request : Request.t) :
     (Authenticated_request.t, 'b, 'b, string) Middleware.Diverter.t ->
  match Authenticated_request.authenticate request with
  | Ok x -> Middleware.continue x (fun b -> b)
  | Error e -> Middleware.stop (Error "Request is not authenticated")
```

Note that Middleware is written in a manner that makes it easy to use with any
monad of one or two parameters, e.g. `Lwt` or `Result`.

## I need more details!

See `lib/middleware.mli` for a fully-documented interface.
