(*open Core*)
open Hardcaml
(*open Signal*)

module I = struct
  type 'a t =
    { din : 'a [@bits 16] }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
      { dout : 'a [@bits 16] }
  [@@deriving hardcaml]
end


(* we assume all inputs are 4 chars for now, we can handle other cases later *)
let create _scope ({ din } : _ I.t) : _ O.t =
    {
        dout = din;
    }

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"parse_ascii" create
;;
