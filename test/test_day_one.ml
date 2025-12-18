open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module Day_one = Aoc_day1.Day_one

module Harness =
  Cyclesim_harness.Make (Day_one.I) (Day_one.O)

let ( <--. ) = Bits.( <--. )

let raw_inputs =
    [ "L68"
    ; "L30"
    ; "R48"
    ; "L5"
    ; "R60"
    ; "L55"
    ; "L1"
    ; "L99"
    ; "R14"
    ; "L82" ];;

(*let raw_inputs = ["R123";"R1";"R45";"L999";"L9";"L76";"R999";"L420"];;*)
let simple_testbench
    ~(inputs : Bits.t ref Day_one.I.t)
    ~(outputs : Bits.t ref Day_one.O.t)
    (sim : Harness.Sim.t)
  =
  let cycle () = Cyclesim.cycle sim in
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
    cycle ();
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

  inputs.clear <--. 1;
  cycle ();
  inputs.clear <--. 0;
  cycle ();
  List.iter ~f:(set) raw_inputs;

  for _ = 1 to 7 do
    cycle ()
  done;
  let number = Bits.to_int_trunc !(outputs.number) in
  Core.printf "%-6d | %-6d | %-6s\n" 0 number "";
;;

let%expect_test "AND gate truth table" =
  Harness.run ~create:Day_one.hierarchical simple_testbench;
  [%expect
    {| |}]
;;

let%expect_test "AND gate with printed waveforms" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "*" |> Re.compile)
    ]
  in

  Harness.run
    ~create:Day_one.hierarchical
    ~trace:`Ports_only
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~start_cycle:((List.length raw_inputs))
        ~signals_width:20
        ~display_width:100
        ~wave_width:2
        waves)
    simple_testbench;

  [%expect
    {| |}]
;;

