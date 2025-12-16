open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module Parse_ascii = Aoc_day1.Parse_ascii

module Harness =
  Cyclesim_harness.Make (Parse_ascii.I) (Parse_ascii.O)

let ( <--. ) = Bits.( <--. )

let raw_inputs = ["R123";"L000";"L999";"R000";"R999";"L420"];;
let simple_testbench
    ~(inputs : Bits.t ref Parse_ascii.I.t)
    ~(outputs : Bits.t ref Parse_ascii.O.t)
    (sim : Harness.Sim.t)
  =
  let step () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.output_valid) then begin
      let r = Bits.to_int_trunc !(outputs.was_r) in
      let number = Bits.to_int_trunc !(outputs.number) in
      Core.printf "%-6d | %-6d | %-6s\n" r number ""
  end in
  let bits_of_ascii str =
    assert (String.length str = 4);
    let open Bits in
    concat_msb (List.init 4 ~f:(fun i -> of_int_trunc ~width:8 (Char.to_int str.[i]))) in
  let set raw_ascii =
    inputs.din := (bits_of_ascii raw_ascii); 
    inputs.d_valid <--. 1;
    step ();
    inputs.d_valid <--. 0;
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
  step ();
  inputs.clear <--. 0;
  step ();
  List.iter ~f:(set) raw_inputs;
  step ();
  step ();
  step ();
  step ();
  step ();
  step ();
;;

let%expect_test "AND gate truth table" =
  Harness.run ~create:Parse_ascii.hierarchical simple_testbench;
  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | R123   |
    1      | L000   |
    2      | L999   |
    3      | R000   |
    4      | R999   |
    5      | L420   |

    Test outputs
    R      | number | Notes
    1      | 123    |
    0      | 0      |
    0      | 999    |
    1      | 0      |
    1      | 999    |
    0      | 420    |
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
    ~create:Parse_ascii.hierarchical
    ~trace:`Ports_only
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:20
        ~display_width:110
        ~wave_width:2
        waves)
    simple_testbench;

  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | R123   |
    1      | L000   |
    2      | L999   |
    3      | R000   |
    4      | R999   |
    5      | L420   |

    Test outputs
    R      | number | Notes
    1      | 123    |
    0      | 0      |
    0      | 999    |
    1      | 0      |
    1      | 999    |
    0      | 420    |
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │clear             ││──────┐                                                                                 │
    │                  ││      └─────────────────────────────────────────────────────────────────────────────    │
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │d_valid           ││            ┌───────────────────────────────────┐                                       │
    │                  ││────────────┘                                   └───────────────────────────────────    │
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────────────────────────────────────────    │
    │din               ││ 0          │1378.│1278.│1278.│1378.│1379.│1278489136                                   │
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────────────────────────────────────────    │
    │                  ││──────────────────────────────┬───────────┬─────┬─────┬─────┬─────┬─────┬───────────    │
    │number            ││ 0                            │560        │123  │0    │999  │0    │999  │420            │
    │                  ││──────────────────────────────┴───────────┴─────┴─────┴─────┴─────┴─────┴───────────    │
    │output_valid      ││                                          ┌───────────────────────────────────┐         │
    │                  ││──────────────────────────────────────────┘                                   └─────    │
    │was_r             ││                                          ┌─────┐           ┌───────────┐               │
    │                  ││──────────────────────────────────────────┘     └───────────┘           └───────────    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

