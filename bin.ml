(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Execute this with one of the example text files like so:
    [dune exec ./bin.exe < example_shoreline.txt] *)

let height = ref 16
let width = ref 64
let filename = ref ""
let rotations = ref "rotations_examples.txt"
let overlap = ref None
let retries = ref 24
let () = Random.self_init ()

let () =
  let open Arg in
  let usage_msg =
    "Read text from stdin and write procedurally generated text to stdout that \
     looks like it."
  in
  let args =
    [
      ( "--file",
        Set_string filename,
        " The path to the file to use as input. Default: stdin" );
      ( "--height",
        Set_int height,
        " The output height in characters. Default: " ^ string_of_int !height );
      ( "--width",
        Set_int width,
        " The output width in characters. Default: " ^ string_of_int !width );
      ( "--overlap",
        Int (fun i -> overlap := Some i),
        " Set this to 2 or greater to enable the overlapping model. It \
         determines how large the overlapping patterns are. Prefer 2 or 3. \
         Default: None" );
      ( "--rotations",
        Set_string rotations,
        " The configuration file for tile rotations. Default: " ^ !rotations );
      ( "--retries",
        Set_int retries,
        " The number of times the algorithm will retry if there's an error. \
         Default: " ^ string_of_int !retries );
      ( "--seed",
        Int Random.init,
        " An integer seed for the random number generator. This is useful for \
         debugging. Default: None (random)" );
    ]
  in
  parse (align args) invalid_arg usage_msg

let str_to_utf_8_list =
  let rec loop str idx =
    match String.get_utf_8_uchar str idx with
    | exception Invalid_argument _ -> []
    | decode ->
        let len = Uchar.utf_decode_length decode in
        let char = Uchar.utf_decode_uchar decode in
        char :: loop str (idx + len)
  in
  fun str -> loop str 0

let rec parse_lines chan =
  match In_channel.input_line chan with
  | None -> []
  | Some str -> str_to_utf_8_list str :: parse_lines chan

let input =
  (match !filename with
  | "" -> parse_lines stdin
  | filename -> In_channel.with_open_text filename parse_lines)
  |> List.map Array.of_list |> Array.of_list

let transforms =
  let module T = Wave_function_collapse.Transforms in
  let scan_charlist ic =
    try Scanf.bscanf ic ":%[^\n]" str_to_utf_8_list
    with End_of_file -> failwith "unexpected end of file."
  in
  let scan_line map ic =
    Scanf.bscanf ic " %c" @@ function
    | '#' ->
        Scanf.bscanf ic "%_[^\n]" ();
        map
    | 'X' -> (
        Scanf.bscanf ic " %r" scan_charlist @@ function
        | [ r0 ] -> T.add_symmetry_X ~r0 map
        | _ -> failwith "X symmetries must have exactly 1 character.")
    | 'T' -> (
        Scanf.bscanf ic " %r" scan_charlist @@ function
        | [ r0; r90; r180; r270 ] -> T.add_symmetry_T ~r0 ~r90 ~r180 ~r270 map
        | _ -> failwith "T symmetries must have exactly 4 characters.")
    | 'L' -> (
        Scanf.bscanf ic " %r" scan_charlist @@ function
        | [ r0; r90; r180; r270 ] -> T.add_symmetry_L ~r0 ~r90 ~r180 ~r270 map
        | _ -> failwith "L symmetries must have exactly 4 characters.")
    | 'I' -> (
        Scanf.bscanf ic " %r" scan_charlist @@ function
        | [ r0; r90 ] -> T.add_symmetry_I ~r0 ~r90 map
        | _ -> failwith "I symmetries must have exactly 2 characters.")
    | '/' -> (
        Scanf.bscanf ic " %r" scan_charlist @@ function
        | [ r0; r90 ] -> T.add_symmetry_slash ~r0 ~r90 map
        | _ -> failwith "/ symmetries must have exactly 2 characters.")
    | c -> failwith ("unknown symmetry '" ^ Char.escaped c ^ "'")
  in
  match !rotations with
  | "" -> T.empty
  | rotations ->
      let ic = Scanf.Scanning.open_in rotations in
      let rec scan_config map =
        match Scanf.bscanf ic "%r" (scan_line map) Fun.id with
        | map -> scan_config map
        | exception End_of_file ->
            Scanf.Scanning.close_in ic;
            map
      in
      scan_config T.empty

let blank = Uchar.of_char ' '
let output = Array.make_matrix !height !width blank

let print_matrix matrix =
  let height, width = (Array.length matrix, Array.length matrix.(0)) in
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to pred height do
    for x = 0 to pred width do
      Buffer.add_utf_8_uchar buf matrix.(y).(x)
    done;
    Buffer.add_char buf '\n'
  done;
  Buffer.output_buffer stdout buf

(* We could possibly animate these iterations. *)
let rec run_loop collapse state =
  match collapse state with None -> () | Some state -> run_loop collapse state

let () =
  let module W = Wave_function_collapse in
  let run =
    match !overlap with
    | None ->
        let state =
          W.parse_matrix_simple ~transforms input |> W.Simple.make output
        in
        fun () -> run_loop W.Simple.collapse state
    | Some size ->
        let state =
          W.parse_matrix_overlap ~transforms size input
          |> W.Overlapping.make output
        in
        fun () -> run_loop W.Overlapping.collapse state
  in
  let rec retry n =
    Array.iter (fun row -> Array.fill row 0 (Array.length row) blank) output;
    try
      run ();
      print_matrix output
    with W.Contradiction { y; x } ->
      if n = 0 then (
        Printf.printf
          "The wave function collapse algorithm failed after %i tries.\n\
           Here was its final attempt, with a contradiction at row %i and \
           column %i:\n"
          (succ !retries) y x;
        print_matrix output;
        exit 1)
      else (
        Printf.printf
          "The wave function collapse algorithm failed. Retrying...\n";
        retry (pred n))
  in
  retry !retries
