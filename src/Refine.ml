type 'prop prop = [>
  | `emp
  | `sep of 'prop * 'prop
  | `points_to of Syntax.var * Syntax.value
  | `exists of Syntax.var * Syntax.typ * 'prop
  | `undefined of Syntax.var
  | `imp of 'prop * 'prop
] as 'prop

module type Kernel = sig
  type 'prop witness constraint 'prop = 'prop prop

  val seq : 'prop witness -> 'prop witness -> 'prop witness

  val alloca : Syntax.var -> 'prop witness -> 'prop witness

  val cons : Syntax.var -> Syntax.value -> 'prop witness

  val move : Syntax.var -> Syntax.var -> 'prop witness

  val dispose : Syntax.var -> 'prop witness
end

module type S = sig
  module Kernel : Kernel

  type 'prop goal = {
    pre : 'prop;
    post : 'prop;
  } constraint 'prop = 'prop prop

  type 'prop validation = 'prop Kernel.witness list -> 'prop Kernel.witness

  type 'prop rule = 'prop goal -> 'prop goal list * 'prop validation

  val seq : 'prop -> 'prop rule

  val alloca : Syntax.var -> 'prop rule

  val cons : 'prop rule

  val move : 'prop rule

  val dispose : 'prop rule
end

module Make (K : Kernel)
  : S with type 'prop Kernel.witness = 'prop K.witness = struct
  module Kernel = K

  type 'prop goal = {
    pre : 'prop;
    post : 'prop;
  } constraint 'prop = 'prop prop

  type 'prop validation = 'prop K.witness list -> 'prop K.witness

  type 'prop rule = 'prop goal -> 'prop goal list * 'prop validation

  let seq mid { pre; post } =
    [{ pre; post = mid }; { pre = mid; post }],
    function
    | [s1; s2] -> K.seq s1 s2
    | _ -> failwith "Validation failed!"

  let alloca v { pre; post } =
    [{ pre = `sep(`undefined v, pre); post = `sep(`undefined v, post) }],
    function
    | [s] -> K.alloca v s
    | _ -> failwith "Validation failed!"

  let cons = function
    | { pre = `undefined var1; post = `points_to(var2, value) }
      when var1 = var2 ->
      [], (function
          | [] -> K.cons var1 value
          | _ -> failwith "Validation failed!")
    | _ -> failwith "Rule does not apply!"

  let move = function
    | { pre = `sep(`points_to(var1, value), `undefined var2)
      ; post = `sep(`undefined var1', `points_to(var2', value')) }
      when var1 = var1' && var2 = var2' && value = value' ->
      [], (function
          | [] -> K.move var2 var1
          | _ -> failwith "Validation failed!")
    | _ -> failwith "Rule does not apply!"

  let dispose = function
    | { pre = `points_to(v1, _); post = `undefined v2 } when v1 = v2 ->
      [], (function
          | [] -> K.dispose v1
          | _ -> failwith "Validation failed!")
    | _ -> failwith "Rule does not apply!"
end
