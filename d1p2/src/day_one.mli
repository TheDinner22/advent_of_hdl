open Hardcaml

module I : sig
  type 'a t =
    { din : 'a [@bits 32] (* 4*8=32 *)
    ; clock : 'a
    ; clear : 'a
    ; d_valid : 'a
    } 
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
      { number : 'a [@bits 16] }
  [@@deriving hardcaml]
end

(*val create : Signal.t I.t -> Signal.t O.t*)
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
