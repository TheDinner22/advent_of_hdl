open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module Door_dial = Aoc_day1.Door_dial

module Harness =
  Cyclesim_harness.Make (Door_dial.I) (Door_dial.O)

let ( <--. ) = Bits.( <--. )

let raw_inputs = [50; 100; 200; 300];;

let simple_testbench
    ~(inputs : Bits.t ref Door_dial.I.t)
    ~(outputs : Bits.t ref Door_dial.O.t)
    (sim : Harness.Sim.t)
  =
  let cycle () = Cyclesim.cycle sim in
  let bits_of_number number =
      let open Bits in
      of_int_trunc ~width:16 number in
  let set raw_number =
    inputs.din := (bits_of_number raw_number); 
    inputs.valid := Bits.of_string "1";
    inputs.r := Bits.of_string "1";
    cycle ();
    let number = Bits.to_int_trunc !(outputs.result) in
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
  Core.printf "%-6s | %-6s | %-6s\n" "R" "number" "Notes";

  inputs.clear <--. 1;
  cycle ();
  inputs.clear <--. 0;
  cycle ();
  List.iter ~f:(set) raw_inputs;
  inputs.valid := Bits.of_string "0";
  cycle ();
  cycle ();
  cycle ();
  cycle ();
  cycle ();
  cycle ();
  cycle ();
  cycle ();
;;

let%expect_test "AND gate truth table" =
  Harness.run ~create:Door_dial.hierarchical simple_testbench;
  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | 50     |
    1      | 100    |
    2      | 200    |
    3      | 300    |

    Test outputs
    R      | number | Notes
           | 1      |
           | 2      |
           | 4      |
           | 7      |
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
    ~create:Door_dial.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:50
        ~display_width:100
        ~wave_width:2
        waves)
    simple_testbench;

  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | 50     |
    1      | 100    |
    2      | 200    |
    3      | 300    |

    Test outputs
    R      | number | Notes
           | 1      |
           | 2      |
           | 4      |
           | 7      |
    ┌Signals─────────────────────────────────────────┐┌Waves───────────────────────────────────────────┐
    │clear                                           ││──────┐                                         │
    │                                                ││      └─────────────────────────────────────────│
    │clock                                           ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                                                ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │                                                ││────────────┬─────┬─────┬─────┬─────────────────│
    │din                                             ││ 0          │50   │100  │200  │300              │
    │                                                ││────────────┴─────┴─────┴─────┴─────────────────│
    │r                                               ││            ┌───────────────────────────────────│
    │                                                ││────────────┘                                   │
    │valid                                           ││            ┌───────────────────────┐           │
    │                                                ││────────────┘                       └───────────│
    │                                                ││──────────────────┬─────┬─────┬─────┬───────────│
    │result                                          ││ 0                │1    │2    │4    │7          │
    │                                                ││──────────────────┴─────┴─────┴─────┴───────────│
    │                                                ││──────────────────┬─────┬─────┬─────┬───────────│
    │parse_ascii$counter_reg_out                     ││ 0                │1    │2    │4    │7          │
    │                                                ││──────────────────┴─────┴─────┴─────┴───────────│
    │                                                ││──────┬───────────┬─────────────────────────────│
    │parse_ascii$dial_reg_out                        ││ 0    │50         │0                            │
    │                                                ││──────┴───────────┴─────────────────────────────│
    │parse_ascii$i$clear                             ││──────┐                                         │
    │                                                ││      └─────────────────────────────────────────│
    │parse_ascii$i$clock                             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                                                ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │                                                ││────────────┬─────┬─────┬─────┬─────────────────│
    │parse_ascii$i$din                               ││ 0          │50   │100  │200  │300              │
    │                                                ││────────────┴─────┴─────┴─────┴─────────────────│
    │parse_ascii$i$r                                 ││            ┌───────────────────────────────────│
    │                                                ││────────────┘                                   │
    │parse_ascii$i$valid                             ││            ┌───────────────────────┐           │
    │                                                ││────────────┘                       └───────────│
    │                                                ││──────┬─────┬───────────┬─────┬─────────────────│
    │parse_ascii$mod100$parse_ascii$i$din            ││ 0    │50   │100        │200  │300              │
    │                                                ││──────┴─────┴───────────┴─────┴─────────────────│
    │                                                ││──────┬─────┬───────────────────────────────────│
    │parse_ascii$mod100$parse_ascii$o$dout           ││ 0    │50   │0                                  │
    │                                                ││──────┴─────┴───────────────────────────────────│
    │                                                ││────────────┬───────────┬─────┬─────────────────│
    │parse_ascii$mod100$parse_ascii$o$times_passed_ze││ 0          │1          │2    │3                │
    │                                                ││────────────┴───────────┴─────┴─────────────────│
    │                                                ││──────────────────┬─────┬─────┬─────┬───────────│
    │parse_ascii$o$result                            ││ 0                │1    │2    │4    │7          │
    │                                                ││──────────────────┴─────┴─────┴─────┴───────────│
    └────────────────────────────────────────────────┘└────────────────────────────────────────────────┘
    |}]
;;

