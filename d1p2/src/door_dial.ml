(*open Core*)
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { din : 'a [@bits 16]
    ; r : 'a
    ; valid : 'a
    ; clock : 'a
    ; clear : 'a
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
      { result : 'a [@bits 16] }
  [@@deriving hardcaml]
end


(* we assume all inputs are 4 chars for now, we can handle other cases later *)
let create scope ({ din; r; valid; clock; clear } : _ I.t) : _ O.t =
    let spec = Signal.Reg_spec.create ~clock ~clear () in
    let%hw dial_reg_out = Signal.wire 16 in
    let%hw counter_reg_out = Signal.wire 16 in

    let value_to_mod = mux2 r (dial_reg_out +: din) ( dial_reg_out -: din) in
    let mod_100_scope = Scope.sub_scope scope "mod100" in
    let mod_out = 
        Mod_hundred.hierarchical 
            mod_100_scope 
            { din = value_to_mod } in

    let dial_reg_zero = dial_reg_out ==: of_int_trunc ~width:16 0 in
    let would_double_count = dial_reg_zero &: (any_bit_set mod_out.times_passed_zero) in
    let next_couter_reg = (counter_reg_out -: (uresize ~width:16 would_double_count) +: (uresize ~width:16 dial_reg_zero) +: (uresize ~width:16 mod_out.times_passed_zero)) in

    let dr = Signal.reg spec ~enable:valid ~clear_to:(of_int_trunc ~width:16 50) mod_out.dout in
    let cr = Signal.reg spec ~enable:valid next_couter_reg in

    Signal.(dial_reg_out <-- dr);
    Signal.(counter_reg_out <-- cr);
    {
        result = counter_reg_out;
    }

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"parse_ascii" create
;;
