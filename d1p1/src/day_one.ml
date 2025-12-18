(*open Core*)
open Hardcaml
(*open Signal*)

module I = struct
  type 'a t =
    { din : 'a [@bits 32] (* 4*8=32 *)
    ; clock : 'a
    ; clear : 'a
    ; d_valid : 'a
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
      { number : 'a [@bits 16] }
  [@@deriving hardcaml]
end

let create scope ({ din; clock; clear; d_valid } : _ I.t) : _ O.t =
    let parse_ascii_scope = Scope.sub_scope scope "parse" in
    let parse_ascii_out = 
        Parse_ascii.hierarchical 
            parse_ascii_scope 
            { din
            ; clock
            ; clear
            ; d_valid } in

    let door_dial_scope = Scope.sub_scope scope "dial" in
    let door_dial_out = 
        Door_dial.hierarchical 
            door_dial_scope 
            { din   = parse_ascii_out.number
            ; r     = parse_ascii_out.was_r
            ; valid = parse_ascii_out.output_valid
            ; clock
            ; clear } in

    { number = door_dial_out.result; };;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"parse_ascii" create
;;
