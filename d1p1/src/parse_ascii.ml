open Core
open Hardcaml
open Signal

let input_width  = 32
let output_width = 16

module I = struct
  type 'a t =
    { din : 'a [@bits input_width] (* 4*8=32 *)
    ; clock : 'a
    ; clear : 'a
    ; d_valid: 'a
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
      { number : 'a [@bits output_width]
      ; was_r  : 'a (* 1 for R, 0 for L*)
      ; output_valid : 'a
      }
  [@@deriving hardcaml]
end


(* we assume all inputs are 4 chars for now, we can handle other cases later *)
let create _scope ({ din; clock; clear; d_valid } : _ I.t) : _ O.t =
    let spec = Signal.Reg_spec.create ~clock ~clear () in
    let ascii_zero = of_int_trunc ~width:8 (Char.to_int '0') in
    let binary_zero = of_int_trunc ~width:8 0 in
    let raw_chars = split_msb ~part_width:8 din in
    let packed_chars =
        match raw_chars with
        | [a; b; c; d] ->
                let is_2_chars = 
                    (a ==: binary_zero) &:
                    (b ==: binary_zero) in
                let is_3_chars = 
                    a ==: binary_zero in
                let sel = is_3_chars @: is_2_chars in
                (*mux cases:
                    is_3_chars | is_2_chars | desired output
                    0          | 0          | chars=raw_chars
                    0          | 1          | not possible, output = don't care (just set chars=raw_chars)
                    1          | 0          | chars=[b, ascii_zero, c, d]
                    1          | 1          | chars[c, ascii_zero, ascii_zero, d]=
                *)
                cases sel 
                ~default:(din)
                [
                      of_string "10", concat_msb [b; ascii_zero; c; d]
                    ; of_string "11", concat_msb [c; ascii_zero; ascii_zero; d]
                ]
        | _ -> assert false in
    let chars = split_msb ~part_width:8 packed_chars in

    (* stage 1 register raw ascii char inputs and pipeline the d_valid signal *)
    let chars_r = List.map ~f:(Signal.reg spec) chars in 
    let valid_pipeline = Signal.pipeline spec ~enable:Signal.vdd ~n:5 d_valid in

    (* ideally we leave in list and keep doing operations
       on it, but char1 needs something different from char2
       so we have to pull them out *)
    let (c3_r, c2_r, c1_r, c0_r) = 
        match chars_r with
        | [c3_r; c2_r; c1_r; c0_r] -> c3_r, c2_r, c1_r, c0_r
        | _ -> assert false 
    in

    (* between stages 1 and 2, comb logic to 
       convert ascii to binary, or R to 1 other to 0
       no registers here                            *)
    let c3_r_is_R = c3_r ==: of_int_trunc ~width:8 (Char.to_int 'R') in
    (* we will need to resize later so no point compressing to 4 bits now *)
    let c2_r_as_bcd = c2_r -: ascii_zero in
    let c1_r_as_bcd = c1_r -: ascii_zero in
    let c0_r_as_bcd = c0_r -: ascii_zero in

    (* stage 2: register the previous comb logic, and save some pain on R/L signal *)

    (*we just need to delay the c3_r_is_R signal and then finally use it as an output*)
    let c3_r_is_R_delay_4 = Signal.pipeline spec ~enable:Signal.vdd ~n:4 c3_r_is_R in

    (* same is true for the least sig char, which doesn't need bcd->binary conversion
       and isn't needed until the last stage of the pipeline*)
    let c0_bcd_delay_3 = Signal.pipeline spec ~enable:Signal.vdd ~n:3 c0_r_as_bcd in

    (* rest just get another dff *)
    let c2_r_as_bcd_r = Signal.reg spec c2_r_as_bcd in
    let c1_r_as_bcd_r = Signal.reg spec c1_r_as_bcd in

    (* between stages 2 and 3, comb logic for
       BCD to binary, no need to worry about RL signal
       multiply digits by respective powers of 10
       *)
    
    let c2_as_binary = uresize (c2_r_as_bcd_r *: (of_int_trunc ~width:10 100)) ~width:10 in
    let c1_as_binary = uresize (c1_r_as_bcd_r *: (of_int_trunc ~width:10 10)) ~width:10 in

    (* stage 3! almost done! register the three values :)*)
    let c2_as_binary_r = Signal.reg spec c2_as_binary in
    let c1_as_binary_r = Signal.reg spec c1_as_binary in

    (* between stages 3-4! lets goooo 
       just add the previous two reg's *)
    let c2_plus_c1 = uresize (c2_as_binary_r +: c1_as_binary_r) ~width:11 in

    (*stage 4!*)
    let c2_plus_c1 = Signal.reg spec c2_plus_c1 in

    (*between stages 4 and 5! one more sum to go!*)
    let final_sum = uresize (c2_plus_c1 +: uresize c0_bcd_delay_3 ~width:11) ~width:output_width in

    (*stage 5!*)
    let final_sum_r = Signal.reg spec final_sum in
    {
        number = final_sum_r;
        was_r  = c3_r_is_R_delay_4;
        output_valid = valid_pipeline;
    };;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"parse_ascii" create
;;
