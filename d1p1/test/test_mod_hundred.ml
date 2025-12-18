open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module Mod_hundred = Aoc_day1.Mod_hundred

module Harness =
  Cyclesim_harness.Make (Mod_hundred.I) (Mod_hundred.O)

(*let ( <--. ) = Bits.( <--. )*)

let raw_inputs = [123;1;-45;999;9;-100;-200;420];;
let simple_testbench
    ~(inputs : Bits.t ref Mod_hundred.I.t)
    ~(outputs : Bits.t ref Mod_hundred.O.t)
    (sim : Harness.Sim.t)
  =
  let cycle () = Cyclesim.cycle sim in
  let bits_of_number num =
      let open Bits in
      of_int_trunc ~width:16 num in
  let set raw_number =
    inputs.din := (bits_of_number raw_number); 
    cycle ();
    let number = Bits.to_int_trunc !(outputs.dout) in
    Core.printf "%-6s | %-6d | %-6s\n" "" number "";
  in

  (* print header *)
  Core.printf "Raw Inputs\n";
  Core.printf "%-6s | %-6s | %-6s\n" "Index" "Input" "Notes";
  Core.printf "-------+--------+--------\n";
  List.iteri raw_inputs ~f:(fun i s ->
    Core.printf "%-6d | %-6d | %-6s\n" i s ""
  );
  Core.printf "\n";

  Core.printf "Test outputs\n";
  Core.printf "%-6s | %-6s | %-6s\n" "hi" "number" "Notes";

  List.iter ~f:(set) raw_inputs;
  cycle ();
  cycle ();
;;

let%expect_test "AND gate truth table" =
  Harness.run ~create:Mod_hundred.hierarchical simple_testbench;
  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | 123    |
    1      | 1      |
    2      | -45    |
    3      | 999    |
    4      | 9      |
    5      | -100   |
    6      | -200   |
    7      | 420    |

    Test outputs
    hi     | number | Notes
           | 23     |
           | 1      |
           | 55     |
           | 99     |
           | 9      |
           | 0      |
           | 0      |
           | 20     |
    |}]
;;

let%expect_test "AND gate with printed waveforms" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "*" |> Re.compile)
    ]
  in

  Harness.run
    ~create:Mod_hundred.hierarchical
    ~trace:`Ports_only
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:20
        ~display_width:120
        ~wave_width:2
        waves)
    simple_testbench;

  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | 123    |
    1      | 1      |
    2      | -45    |
    3      | 999    |
    4      | 9      |
    5      | -100   |
    6      | -200   |
    7      | 420    |

    Test outputs
    hi     | number | Notes
           | 23     |
           | 1      |
           | 55     |
           | 99     |
           | 9      |
           | 0      |
           | 0      |
           | 20     |
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────────────────                                      │
    │din               ││ 123  │1    │65491│999  │9    │65436│65336│420                                                    │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────────────────                                      │
    │                  ││──────┬─────┬─────┬─────┬─────┬───────────┬─────────────────                                      │
    │dout              ││ 23   │1    │55   │99   │9    │0          │20                                                     │
    │                  ││──────┴─────┴─────┴─────┴─────┴───────────┴─────────────────                                      │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

