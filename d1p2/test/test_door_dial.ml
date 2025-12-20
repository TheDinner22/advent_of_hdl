open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module Door_dial = Aoc_day1.Door_dial

module Harness =
  Cyclesim_harness.Make (Door_dial.I) (Door_dial.O)

let ( <--. ) = Bits.( <--. )

(*let raw_inputs = ["R1"; "L1"; "R1"; "L1"];;*)

let raw_inputs =
[ "L250"
; "L250"
; "L1"
; "L1"
; "L1"
; "L1"
; "L1"
; "L1"
; "L1" ]

let simple_testbench
    ~(inputs : Bits.t ref Door_dial.I.t)
    ~(outputs : Bits.t ref Door_dial.O.t)
    (sim : Harness.Sim.t)
  =
  let cycle () = Cyclesim.cycle sim in
  let bits_of_number number =
      let open Bits in
      of_int_trunc ~width:16 number in
  let set raw_string =
    let open Bits in
    inputs.din := bits_of_number @@ int_of_string @@ (String.sub raw_string ~pos:1 ~len:(String.length raw_string - 1)); 
    inputs.valid := Bits.of_string "1";
    inputs.r := (of_int_trunc ~width:8 @@ Char.to_int @@ raw_string.[0]) ==: (of_int_trunc ~width:8 @@ Char.to_int @@ 'R');
    cycle ();
    let number = Bits.to_int_trunc !(outputs.result) in
    Core.printf "%-6s | %-6d | %-6s\n" "" number "";
  in

  (* print header *)
  Core.printf "Raw Inputs\n";
  Core.printf "%-6s | %-6s | %-6s\n" "Index" "Input" "Notes";
  Core.printf "-------+--------+--------\n";
  List.iteri raw_inputs ~f:(fun i s ->
    Core.printf "%-6d | %-6s | %-6s\n" i s ""
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
    0      | L250   |
    1      | L250   |
    2      | L1     |
    3      | L1     |
    4      | L1     |
    5      | L1     |
    6      | L1     |
    7      | L1     |
    8      | L1     |

    Test outputs
    R      | number | Notes
           | 3      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
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
        ~signals_width:40
        ~display_width:120
        ~wave_width:2
        waves)
    simple_testbench;

  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | L250   |
    1      | L250   |
    2      | L1     |
    3      | L1     |
    4      | L1     |
    5      | L1     |
    6      | L1     |
    7      | L1     |
    8      | L1     |

    Test outputs
    R      | number | Notes
           | 3      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
           | 5      |
    ┌Signals───────────────────────────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clear                                 ││──────┐                                                                       │
    │                                      ││      └───────────────────────────────────────────────────────────────────────│
    │clock                                 ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                                      ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │                                      ││────────────┬───────────┬─────────────────────────────────────────────────────│
    │din                                   ││ 0          │250        │1                                                    │
    │                                      ││────────────┴───────────┴─────────────────────────────────────────────────────│
    │r                                     ││                                                                              │
    │                                      ││──────────────────────────────────────────────────────────────────────────────│
    │valid                                 ││            ┌─────────────────────────────────────────────────────┐           │
    │                                      ││────────────┘                                                     └───────────│
    │                                      ││──────────────────┬─────┬─────────────────────────────────────────────────────│
    │result                                ││ 0                │3    │5                                                    │
    │                                      ││──────────────────┴─────┴─────────────────────────────────────────────────────│
    │                                      ││──────────────────┬─────┬─────────────────────────────────────────────────────│
    │parse_ascii$counter_reg_out           ││ 0                │3    │5                                                    │
    │                                      ││──────────────────┴─────┴─────────────────────────────────────────────────────│
    │                                      ││──────┬───────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────│
    │parse_ascii$dial_reg_out              ││ 0    │50         │0    │50   │49   │48   │47   │46   │45   │44   │43         │
    │                                      ││──────┴───────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────│
    │parse_ascii$dial_reg_zero             ││──────┐           ┌─────┐                                                     │
    │                                      ││      └───────────┘     └─────────────────────────────────────────────────────│
    │parse_ascii$i$clear                   ││──────┐                                                                       │
    │                                      ││      └───────────────────────────────────────────────────────────────────────│
    │parse_ascii$i$clock                   ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                                      ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │                                      ││────────────┬───────────┬─────────────────────────────────────────────────────│
    │parse_ascii$i$din                     ││ 0          │250        │1                                                    │
    │                                      ││────────────┴───────────┴─────────────────────────────────────────────────────│
    │parse_ascii$i$r                       ││                                                                              │
    │                                      ││──────────────────────────────────────────────────────────────────────────────│
    │parse_ascii$i$valid                   ││            ┌─────────────────────────────────────────────────────┐           │
    │                                      ││────────────┘                                                     └───────────│
    │                                      ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────│
    │parse_ascii$mod100$parse_ascii$i$din  ││ 0    │50   │65336│65286│49   │48   │47   │46   │45   │44   │43   │42         │
    │                                      ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────│
    │                                      ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────│
    │parse_ascii$mod100$parse_ascii$o$dout ││ 0    │50   │0    │50   │49   │48   │47   │46   │45   │44   │43   │42         │
    │                                      ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────│
    │                                      ││────────────┬─────┬─────┬─────────────────────────────────────────────────────│
    │parse_ascii$mod100$parse_ascii$o$times││ 0          │2    │3    │0                                                    │
    │                                      ││────────────┴─────┴─────┴─────────────────────────────────────────────────────│
    │parse_ascii$mod_out_zero              ││──────┐     ┌─────┐                                                           │
    │                                      ││      └─────┘     └───────────────────────────────────────────────────────────│
    │                                      ││──────────────────┬─────┬─────────────────────────────────────────────────────│
    │parse_ascii$o$result                  ││ 0                │3    │5                                                    │
    │                                      ││──────────────────┴─────┴─────────────────────────────────────────────────────│
    │                                      ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────│
    │parse_ascii$value_to_mod              ││ 0    │50   │65336│65286│49   │48   │47   │46   │45   │44   │43   │42         │
    │                                      ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────│
    │parse_ascii$would_double_count        ││            ┌───────────┐                                                     │
    │                                      ││────────────┘           └─────────────────────────────────────────────────────│
    │parse_ascii$would_under_count_left    ││            ┌─────┐                                                           │
    │                                      ││────────────┘     └───────────────────────────────────────────────────────────│
    │parse_ascii$would_under_count_right   ││                                                                              │
    │                                      ││──────────────────────────────────────────────────────────────────────────────│
    └──────────────────────────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

