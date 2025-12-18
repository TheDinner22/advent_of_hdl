open Hardcaml

module I : sig
  type 'a t =
    { din : 'a [@bits 16] }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
      { dout : 'a [@bits 16] }
  [@@deriving hardcaml]
end

(*val create : Signal.t I.t -> Signal.t O.t*)
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
