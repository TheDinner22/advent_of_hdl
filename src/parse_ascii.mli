open Hardcaml

module I : sig
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { y : 'a }
  [@@deriving hardcaml]
end

val create : Signal.t I.t -> Signal.t O.t
