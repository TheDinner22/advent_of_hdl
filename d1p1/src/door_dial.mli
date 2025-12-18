open Hardcaml

module I : sig
  type 'a t =
    { din : 'a [@bits 16]
    ; r : 'a
    ; valid : 'a
    ; clock : 'a
    ; clear : 'a
    } 
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
      { result : 'a [@bits 16] }
  [@@deriving hardcaml]
end

(*val create : Signal.t I.t -> Signal.t O.t*)
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
