open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module Parse_ascii = Aoc_day1.Parse_ascii

module Harness =
  Cyclesim_harness.Make (Parse_ascii.I) (Parse_ascii.O)

let ( <--. ) = Bits.( <--. )

let raw_inputs = ["R123";"R1";"R45";"L999";"L9";"L76";"R999";"L420"];;
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
    assert (String.length str <= 4);
    assert (String.length str >= 2);
    let str_len = String.length str in
    let open Bits in
    let zero_byte = of_int_trunc ~width:8 0 in
    (*concat_msb (List.init 4 ~f:(fun i -> of_int_trunc ~width:8 (Char.to_int str.[i]))) in*)
    let char_code_list = String.to_list str
      |> List.map ~f:(fun c -> of_int_trunc ~width:8 @@ Char.to_int c) in
    let padded_char_code_list = 
    match str_len with
    | 2 -> zero_byte :: zero_byte :: char_code_list
    | 3 -> zero_byte :: char_code_list
    | 4 -> char_code_list
    | _ -> assert false in
    concat_msb padded_char_code_list in
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
    1      | R1     |
    2      | R45    |
    3      | L999   |
    4      | L9     |
    5      | L76    |
    6      | R999   |
    7      | L420   |

    Test outputs
    R      | number | Notes
    1      | 123    |
    1      | 1      |
    1      | 45     |
    0      | 999    |
    0      | 9      |
    0      | 76     |
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
        ~display_width:120
        ~wave_width:2
        waves)
    simple_testbench;

  [%expect
    {|
    Raw Inputs
    Index  | Input  | Notes
    -------+--------+--------
    0      | R123   |
    1      | R1     |
    2      | R45    |
    3      | L999   |
    4      | L9     |
    5      | L76    |
    6      | R999   |
    7      | L420   |

    Test outputs
    R      | number | Notes
    1      | 123    |
    1      | 1      |
    1      | 45     |
    0      | 999    |
    0      | 9      |
    0      | 76     |
    1      | 999    |
    0      | 420    |
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clear             ││──────┐                                                                                           │
    │                  ││      └─────────────────────────────────────────────────────────────────────────────────────────  │
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │d_valid           ││            ┌───────────────────────────────────────────────┐                                     │
    │                  ││────────────┘                                               └───────────────────────────────────  │
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────────────────────────────────────────  │
    │din               ││ 0          │1378.│21041│5387.│1278.│19513│4994.│1379.│1278489136                                 │
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────────────────────────────────────────  │
    │                  ││──────────────────────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────  │
    │number            ││ 0                            │560  │208  │123  │1    │45   │999  │9    │76   │999  │420          │
    │                  ││──────────────────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────  │
    │output_valid      ││                                          ┌───────────────────────────────────────────────┐       │
    │                  ││──────────────────────────────────────────┘                                               └─────  │
    │was_r             ││                                          ┌─────────────────┐                 ┌─────┐             │
    │                  ││──────────────────────────────────────────┘                 └─────────────────┘     └───────────  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

