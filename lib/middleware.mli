(** A "middleware" function is one which runs before, after, and possibly instead of another
    function.

    Middleware can be composed, in order to create a chain of wrapping invocations.

    Note that a middleware function can not be called on its own. It is required to be "terminated"
    by a conventional function. *)

(** Minimal monad definition required by a Middleware. *)
module type MONAD = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn a] applies [fn] to [a], producing a ['b t]. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind fn a] sequentially composes [a] with [fn], passing the result of the first action to
      [fn]. *)

  val return : 'a -> 'a t
  (** [return a] injects the value [a] into the monadic type. *)
end

(** Monads of two type parameters. *)
module type MONAD2 = sig
  type ('m, 'a) t

  val map : ('a -> 'b) -> ('a, 'm) t -> ('b, 'm) t
  (** [map fn a] applies [fn] to [a], producing a ['b t]. *)

  val bind : ('a, 'm) t -> ('a -> ('b, 'm) t) -> ('b, 'm) t
  (** [bind fn a] sequentially composes [a] with [fn], passing the result of the first action to
      [fn]. *)

  val return : 'a -> ('a, 'm) t
  (** [return a] injects the value [a] into the monadic type. *)
end

(** Some common monads. *)
module Monads : sig
  module Identity : MONAD with type 'a t = 'a
  (** The identity monad, which has no special monadic behavior. *)
end

(** Creates a Middleware module, providing effects with the given Monad. *)
module Make : functor (M : MONAD) -> sig
  module Diverter : sig
    (** Union, determining whether or not to run a subsequent [Middleware] or return early. *)
    type ('next_input, 'next_output, 'output) t =
      | Stop of 'output M.t
          (** Control stops here, and the ['output] is returned to the wrapping [Middleware]. *)
      | Continue of ('next_input * ('next_output -> 'output M.t))
          (** Control continues to the next [Middleware] [g], which receives ['next_input] as input.
              After [g] runs, its result is transformed with the given function. *)
  end

  val stop : 'output M.t -> (_, _, 'output) Diverter.t
  (** [stop x] creates a [Stop] [Diverter] with the return value [x]. *)

  val continue :
    'next_input ->
    ('next_output -> 'output M.t) ->
    ('next_input, 'next_output, 'output) Diverter.t
  (** [continue next_input fn] creates a [Continue] diverter, passing [next_input] to the subsequent
      [Middleware] [g]. [fn] is applied to the value returned by [g].*)

  type ('input, 'next_input, 'next_output, 'output) t =
    'input -> ('next_input, 'next_output, 'output) Diverter.t
  (** A function which composes with other [Middleware] functions. Composed [Middleware] can respond
      to the return value of [Middleware] running later. When a [Middleware] function [f] runs, it
      either:

      - [Stop]s, and returns a result
      - [Continue]s by allowing the next middleware function [g] to run
      - After [g] runs, control returns to [f], where [f] can respond to [g]'s result and change its
        own return value. *)

  val terminate : ('ai, 'bi, 'bo, 'ao) t -> ('bi -> 'bo M.t) -> 'ai -> 'ao M.t
  (** [terminate ma fb] terminates [Middleware] [ma] by providing a function [fb] which does not
      delegate. In order to turn a [Middleware] chain into a function, it must be terminated by a
      non-delegating function using [terminate]. *)

  val compose :
    ('ai, 'bi, 'bo, 'ao) t -> ('bi, 'ci, 'co, 'bo) t -> ('ai, 'ci, 'co, 'ao) t
  (** [compose ma mb] composes two [Middleware] functions. When run, [ma] first runs, giving [mb]
      the option to continue later on. Note that a composed [Middleware] still need to be terminated
      via [terminate] to be called. *)

  module Infix : sig
    val ( <<>> ) :
      ('ai, 'bi, 'bo, 'ao) t -> ('bi, 'ci, 'co, 'bo) t -> ('ai, 'ci, 'co, 'ao) t
    (** [ma <<>> mb] is an infix [compose] operator. *)

    val ( <&> ) : ('ai, 'bi, 'bo, 'ao) t -> ('bi -> 'bo M.t) -> 'ai -> 'ao M.t
    (** [ma <&> mb] is an infix [terminate] operator. *)
  end
end

(** Creates a Middleware module, providing effects with the given two-parameter Monad. *)
module Make2 : functor (M : MONAD2) -> sig
  module Diverter : sig
    (** Union, determining whether or not to run a subsequent [Middleware] or return early. *)
    type ('next_input, 'next_output, 'output, 'm) t =
      | Stop of ('output, 'm) M.t
          (** Control stops here, and ['output] is returned to the wrapping [Middleware]. *)
      | Continue of ('next_input * ('next_output -> ('output, 'm) M.t))
          (** Control continues to the next [Middleware] [g], which receives ['next_input] as input.
              After [g] runs, its result is transformed with the given function. *)
  end

  val stop : ('output, 'm) M.t -> (_, _, 'output, 'm) Diverter.t
  (** [stop x] creates a [Stop] [Diverter] with the return value [x]. *)

  val continue :
    'next_input ->
    ('next_output -> ('output, 'm) M.t) ->
    ('next_input, 'next_output, 'output, 'm) Diverter.t
  (** [continue next_input fn] creates a [Continue] diverter, passing [next_input] to the subsequent
      [Middleware] [g]. [fn] is applied to the value returned by [g].*)

  type ('input, 'next_input, 'next_output, 'output, 'm) t =
    'input -> ('next_input, 'next_output, 'output, 'm) Diverter.t
  (** A function which composes with other [Middleware] functions. Composed [Middleware] can respond
      to the return value of [Middleware] running later. When a [Middleware] function [f] runs, it
      either:

      - [Stop]s, and returns a result
      - [Continue]s by allowing the next middleware function [g] to run
      - After [g] runs, control returns to [f], where [f] can respond to [g]'s result and change its
        own return value. *)

  val terminate :
    ('ai, 'bi, 'bo, 'ao, 'm) t -> ('bi -> ('bo, 'm) M.t) -> 'ai -> ('ao, 'm) M.t
  (** [terminate ma fb] terminates [Middleware] [ma] by providing a function [fb] which does not
      delegate. In order to turn a [Middleware] chain into a function, it must be terminated by a
      non-delegating function using [terminate]. *)

  val compose :
    ('ai, 'bi, 'bo, 'ao, 'm) t ->
    ('bi, 'ci, 'co, 'bo, 'm) t ->
    ('ai, 'ci, 'co, 'ao, 'm) t
  (** [compose ma mb] composes two [Middleware] functions. When run, [ma] first runs, giving [mb]
      the option to continue later on. Note that a composed [Middleware] still need to be terminated
      via [terminate] to be called. *)

  module Infix : sig
    val ( <<>> ) :
      ('ai, 'bi, 'bo, 'ao, 'm) t ->
      ('bi, 'ci, 'co, 'bo, 'm) t ->
      ('ai, 'ci, 'co, 'ao, 'm) t
    (** [ma <<>> mb] is an infix [compose] operator. *)

    val ( <&> ) :
      ('ai, 'bi, 'bo, 'ao, 'm) t ->
      ('bi -> ('bo, 'm) M.t) ->
      'ai ->
      ('ao, 'm) M.t
    (** [ma <&> mb] is an infix [terminate] operator. *)
  end
end

include module type of Make (Monads.Identity)
(** Middleware backed by the identity monad. *)
