open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module Parse_ascii = Aoc_day1.Parse_ascii

module Harness =
  Cyclesim_harness.Make (Parse_ascii.I) (Parse_ascii.O)

let ( <--. ) = Bits.( <--. )

let simple_testbench
    ~(inputs : Bits.t ref Parse_ascii.I.t)
    ~(outputs : Bits.t ref Parse_ascii.O.t)
    (sim : Harness.Sim.t)
  =
  let cycle () = Cyclesim.cycle sim in

  let bits_of_ascii str =
    assert (String.length str = 4);
    let open Bits in
    concat_msb (List.init 4 ~f:(fun i -> of_int_trunc ~width:8 (Char.to_int str.[i]))) in
  let set raw_ascii =
    inputs.clear <--. 1;
    cycle ();
    inputs.clear <--. 0;
    inputs.din := (bits_of_ascii raw_ascii); 
    cycle ();
    cycle ();
    cycle ();
    cycle ();
    cycle ();
    cycle ();
    let r = Bits.to_int_trunc !(outputs.was_r) in
    let number = Bits.to_int_trunc !(outputs.number) in
    print_s [%message "pipeline result" (raw_ascii : string) (r : int) (number : int)]
  in

  set "R123";
;;

let%expect_test "AND gate truth table" =
  Harness.run ~create:Parse_ascii.hierarchical simple_testbench;
  [%expect
    {||}]
;;

let%expect_test "AND gate with printed waveforms" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "*" |> Re.compile)
    ]
  in

  Harness.run
    ~create:Parse_ascii.hierarchical
    ~trace:`Ports_only
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:40
        ~display_width:100
        ~wave_width:2
        waves)
    simple_testbench;

  [%expect
    {||}]
;;

