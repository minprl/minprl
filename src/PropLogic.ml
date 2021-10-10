type 'prop prop = [>
  | `tt
  | `ff
  | `conj of 'prop * 'prop
  | `disj of 'prop * 'prop
  | `imp of 'prop * 'prop
] as 'prop

type 'prop context = 'prop prop list

module type Kernel = sig
  type 'prop witness constraint 'prop = 'prop prop

  val hyp : int -> 'prop witness

  val tt_intro : 'prop context -> 'prop witness

  val ff_elim : 'prop -> 'prop witness -> 'prop witness

  val conj_intro : 'prop witness -> 'prop witness -> 'prop witness

  val conj_elim1 : 'prop witness -> 'prop witness

  val conj_elim2 : 'prop witness -> 'prop witness

  val disj_intro1 : 'prop -> 'prop witness -> 'prop witness

  val disj_intro2 : 'prop -> 'prop witness -> 'prop witness

  val disj_elim
    : 'prop witness -> 'prop witness -> 'prop witness -> 'prop witness

  val imp_intro : 'prop witness -> 'prop witness

  val imp_elim : 'prop witness -> 'prop witness -> 'prop witness
end

module type S = sig
  module Kernel : Kernel

  type 'prop goal = 'prop list * 'prop prop

  type 'prop validation = 'prop Kernel.witness list -> 'prop Kernel.witness

  type 'prop rule = 'prop goal -> 'prop goal list * 'prop validation

  val hyp : int -> 'prop rule

  val tt_intro : 'prop rule

  val ff_elim : 'prop rule

  val conj_intro : 'prop rule

  val conj_elim1 : 'prop -> 'prop rule

  val conj_elim2 : 'prop -> 'prop rule

  val disj_intro1 : 'prop -> 'prop rule

  val disj_intro2 : 'prop -> 'prop rule

  val disj_elim : 'prop -> 'prop -> 'prop rule

  val imp_intro : 'prop rule

  val imp_elim : 'prop -> 'prop rule
end

module Make (K : Kernel)
  : S with type 'prop Kernel.witness = 'prop K.witness = struct
  module Kernel = K

  type 'prop goal = 'prop list * 'prop prop

  type 'prop validation = 'prop K.witness list -> 'prop K.witness

  type 'prop rule = 'prop goal -> 'prop goal list * 'prop validation

  let hyp i (ctx, p) = [], function
      | [] ->
        if List.nth_opt ctx i = Some p then
          K.hyp i
        else
          failwith "Validation failed!"
      | _ -> failwith "Validation failed!"

  let tt_intro = function
    | ctx, `tt -> [], (function
        | [] -> K.tt_intro ctx
        | _ -> failwith "Validation failed!")
    | _, _ -> failwith "Rule does not apply!"

  let ff_elim (ctx, p) = [ctx, `ff], function
      | [impossible] -> K.ff_elim p impossible
      | _ -> failwith "Validation failed!"

  let conj_intro = function
    | ctx, `conj(p, q) -> [ctx, p; ctx, q], (function
        | [fst; snd] -> K.conj_intro fst snd
        | _ -> failwith "Validation failed!")
    | _, _ -> failwith "Rule does not apply!"

  let conj_elim1 q (ctx, p) = [ctx, `conj(p, q)], function
      | [conj] -> K.conj_elim1 conj
      | _ -> failwith "Validation failed!"

  let conj_elim2 q (ctx, p) = [ctx, `conj(q, p)], function
      | [conj] -> K.conj_elim2 conj
      | _ -> failwith "Validation failed!"

  let disj_intro1 q (ctx, p) = [ctx, `disj(p, q)], function
      | [left] -> K.disj_intro1 q left
      | _ -> failwith "Validation failed!"

  let disj_intro2 q (ctx, p) = [ctx, `disj(q, p)], function
      | [right] -> K.disj_intro2 q right
      | _ -> failwith "Validation failed!"

  let disj_elim p q (ctx, r) =
    [ctx, `disj(p, q); p :: ctx, r; q :: ctx, r], function
      | [disj; left; right] -> K.disj_elim disj left right
      | _ -> failwith "Validation failed!"

  let imp_intro = function
    | ctx, `imp(p, q) -> [p :: ctx, q], (function
        | [body] -> K.imp_intro body
        | _ -> failwith "Validation failed!")
    | _, _ -> failwith "Rule does not apply!"

  let imp_elim q (ctx, p) = [ctx, `imp(q, p); ctx, q], function
      | [fn; arg] -> K.imp_elim fn arg
      | _ -> failwith "Validation failed!"
end
