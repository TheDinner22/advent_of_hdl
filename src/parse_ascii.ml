open Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { y : 'a }
  [@@deriving hardcaml]
end

let create (i : Signal.t I.t) : Signal.t O.t =
  { y = i.a &: i.b }

(* saw this in the demo project idk *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"range_finder" create
;;
