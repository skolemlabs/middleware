module type MONAD = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type MONAD2 = sig
  type ('a, 'm) t

  val map : ('a -> 'b) -> ('a, 'm) t -> ('b, 'm) t
  val bind : ('a, 'm) t -> ('a -> ('b, 'm) t) -> ('b, 'm) t
  val return : 'a -> ('a, 'm) t
end

module Monads = struct
  module Identity : MONAD with type 'a t = 'a = struct
    type 'a t = 'a

    let map fn a = fn a
    let bind a fn = fn a
    let return a = a
  end
end

module Make (M : MONAD) = struct
  module Diverter = struct
    type ('next_input, 'next_output, 'output) t =
      | Stop of 'output M.t
      | Continue of ('next_input * ('next_output -> 'output M.t))
  end

  let stop x = Diverter.Stop x

  let continue (next_input : 'next_input) (fn : 'next_output -> 'output M.t) :
      ('next_input, 'next_output, 'output) Diverter.t =
    Diverter.Continue (next_input, fn)

  type ('input, 'next_input, 'next_output, 'output) t =
    'input -> ('next_input, 'next_output, 'output) Diverter.t

  let terminate :
      type ai ao bi bo. (ai, bi, bo, ao) t -> (bi -> bo M.t) -> ai -> ao M.t =
   fun ma fb (ai : ai) : ao M.t ->
    let ( =<< ) f a = M.bind a f in
    match ma ai with
    | Stop ao -> ao
    | Continue (bi, ao_of_bo) -> ao_of_bo =<< fb bi

  let compose :
      type ai ao bi bo ci co.
      (ai, bi, bo, ao) t -> (bi, ci, co, bo) t -> (ai, ci, co, ao) t =
   fun ma mb (ai : ai) ->
    let ( =<< ) f a = M.bind a f in
    match ma ai with
    | Stop ao -> stop ao
    | Continue (bi, ao_of_bo) -> (
        let mb = mb bi in
        match mb with
        | Stop bo -> stop (ao_of_bo =<< bo)
        | Continue (ci, bo_of_co) ->
            let ao_of_co co = ao_of_bo =<< bo_of_co co in
            continue ci ao_of_co)

  module Infix = struct
    let ( <<>> ) = compose
    let ( <&> ) = terminate
  end
end

module Make2 (M : MONAD2) = struct
  module Diverter = struct
    type ('next_input, 'next_output, 'output, 'm) t =
      | Stop of ('output, 'm) M.t
      | Continue of ('next_input * ('next_output -> ('output, 'm) M.t))
  end

  let stop x = Diverter.Stop x

  let continue (next_input : 'next_input)
      (fn : 'next_output -> ('output, 'm) M.t) :
      ('next_input, 'next_output, 'output, 'm) Diverter.t =
    Diverter.Continue (next_input, fn)

  type ('input, 'next_input, 'next_output, 'output, 'm) t =
    'input -> ('next_input, 'next_output, 'output, 'm) Diverter.t

  let terminate :
      type ai ao bi bo.
      (ai, bi, bo, ao, 'm) t -> (bi -> (bo, 'm) M.t) -> ai -> (ao, 'm) M.t =
   fun ma fb (ai : ai) : (ao, 'm) M.t ->
    let ( =<< ) f a = M.bind a f in
    match ma ai with
    | Stop ao -> ao
    | Continue (bi, ao_of_bo) -> ao_of_bo =<< fb bi

  let compose :
      type ai ao bi bo ci co.
      (ai, bi, bo, ao, 'm) t -> (bi, ci, co, bo, 'm) t -> (ai, ci, co, ao, 'm) t
      =
   fun ma mb (ai : ai) ->
    let ( =<< ) f a = M.bind a f in
    match ma ai with
    | Stop ao -> stop ao
    | Continue (bi, ao_of_bo) -> (
        match mb bi with
        | Stop bo -> stop (ao_of_bo =<< bo)
        | Continue (ci, bo_of_co) ->
            let ao_of_co co = ao_of_bo =<< bo_of_co co in
            continue ci ao_of_co)

  module Infix = struct
    let ( <<>> ) = compose
    let ( <&> ) = terminate
  end
end

include Make (Monads.Identity)
