open Hardcaml

module I : sig
  type 'a t =
    { din : 'a [@bits input_width] (* 4*8=32 *)
    ; clock : 'a
    ; clear : 'a
    ; d_valid : 'a
    } 
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
      { number : 'a [@bits output_width]
      ; was_r  : 'a (* 1 for R, 0 for L*)
      ; output_valid : 'a
      }
  [@@deriving hardcaml]
end

(*val create : Signal.t I.t -> Signal.t O.t*)
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
