open Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { din : 'a [@bits 16] }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
      { dout : 'a [@bits 16]
      ; times_passed_zero : 'a [@bits 4] }
  [@@deriving hardcaml]
end


(* we assume all inputs are 4 chars for now, we can handle other cases later *)
let create _scope ({ din } : _ I.t) : _ O.t =
    (*first we need some logic to *)
    let din_is_pos = din >=+ (of_int_trunc ~width:16 0) in (* i'd expect this not to synthesize into a comparitor *)
    let pos_sel = List.map ~f:(of_int_trunc ~width:16) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000]
              |> List.map ~f:(fun num -> din >=: num)
              |> Signal.concat_msb in
    let pos_times_passed_zero = leading_ones pos_sel in
    let pos_mod_result = priority_select_with_default
        ~default:din
        With_valid.
            [ { valid = pos_sel.:(0); value = din -: (of_int_trunc ~width:16 1000) }
            ; { valid = pos_sel.:(1); value = din -: (of_int_trunc ~width:16 900) }
            ; { valid = pos_sel.:(2); value = din -: (of_int_trunc ~width:16 800) }
            ; { valid = pos_sel.:(3); value = din -: (of_int_trunc ~width:16 700) }
            ; { valid = pos_sel.:(4); value = din -: (of_int_trunc ~width:16 600) }
            ; { valid = pos_sel.:(5); value = din -: (of_int_trunc ~width:16 500) }
            ; { valid = pos_sel.:(6); value = din -: (of_int_trunc ~width:16 400) }
            ; { valid = pos_sel.:(7); value = din -: (of_int_trunc ~width:16 300) }
            ; { valid = pos_sel.:(8); value = din -: (of_int_trunc ~width:16 200) }
            ; { valid = pos_sel.:(9); value = din -: (of_int_trunc ~width:16 100) }
            ] in
    let neg_sel = List.map ~f:(of_int_trunc ~width:16) [-100; -200; -300; -400; -500; -600; -700; -800; -900]
              |> List.map ~f:(fun num -> din <+ num)
              |> Signal.concat_msb in
    let neg_times_passed_zero = leading_ones neg_sel in
    let neg_mod_result = priority_select_with_default
        ~default:(din +: (of_int_trunc ~width:16 100))
        With_valid.
            [ { valid = neg_sel.:(0); value = din +: (of_int_trunc ~width:16 1000) }
            ; { valid = neg_sel.:(1); value = din +: (of_int_trunc ~width:16 900) }
            ; { valid = neg_sel.:(2); value = din +: (of_int_trunc ~width:16 800) }
            ; { valid = neg_sel.:(3); value = din +: (of_int_trunc ~width:16 700) }
            ; { valid = neg_sel.:(4); value = din +: (of_int_trunc ~width:16 600) }
            ; { valid = neg_sel.:(5); value = din +: (of_int_trunc ~width:16 500) }
            ; { valid = neg_sel.:(6); value = din +: (of_int_trunc ~width:16 400) }
            ; { valid = neg_sel.:(7); value = din +: (of_int_trunc ~width:16 300) }
            ; { valid = neg_sel.:(8); value = din +: (of_int_trunc ~width:16 200) }
            ] in
    let mod_result = mux2 din_is_pos pos_mod_result neg_mod_result in
    let times_passed_zero = mux2 din_is_pos pos_times_passed_zero (neg_times_passed_zero +: of_int_trunc ~width:4 1) in
    {
        dout = mod_result;
        times_passed_zero = times_passed_zero;
    }

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"parse_ascii" create
;;
