(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module W = Wave_function_collapse

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

let space = Uchar.of_char ' '
let output = Array.make_matrix 12 32 space

let run f state =
  Random.init 0;
  let rec loop state =
    match f state with None -> () | Some state -> loop state
  in
  loop state

let () =
  print_endline "Simple tiled test 1: generating shoreline succeeds.";
  Array.map (Array.map Uchar.of_char)
    [|
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; '.'; '%'; '%'; ','; ' '; ' ' |];
      [| ' '; '.'; '%'; '~'; '~'; '%'; ','; ' ' |];
      [| '%'; '%'; '~'; '~'; '~'; '~'; '%'; '%' |];
      [| '~'; '~'; '~'; '~'; '~'; '~'; '~'; '~' |];
      [| '~'; '~'; '~'; '~'; '~'; '~'; '~'; '~' |];
    |]
  |> W.parse_matrix_simple |> W.Simple.make output |> run W.Simple.collapse;
  print_matrix output

let () =
  print_newline ();
  print_endline "Simple tiled test 2: a collapse failure.";
  Array.iter (fun row -> Array.fill row 0 (Array.length row) space) output;
  try
    Array.map (Array.map Uchar.of_char)
      [|
        [| ' '; ' '; ' ' |];
        [| ' '; '%'; ' ' |];
        [| '%'; '~'; '%' |];
        [| '~'; '~'; '~' |];
      |]
    |> W.parse_matrix_simple |> W.Simple.make output |> run W.Simple.collapse;
    print_matrix output
  with W.Contradiction { y; x } ->
    Printf.printf "Contradiction found at y=%i x=%i.\n" y x;
    print_matrix output

let () =
  print_newline ();
  print_endline "Overlapping test 1: generating contours succeeds.";
  Array.map (Array.map Uchar.of_char)
    [|
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'X' |];
      [| 'X'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; 'X'; 'X'; 'X'; 'X'; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; 'X'; 'X'; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'X'; 'X' |];
      [| 'X'; 'X'; 'X'; 'X'; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; 'X'; 'X'; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; ' '; 'X'; 'X'; ' ' |];
    |]
  |> W.parse_matrix_overlap 3 |> W.Overlapping.make output
  |> run W.Overlapping.collapse;
  print_matrix output

let () =
  print_newline ();
  print_endline "Overlapping test 2: generating rotating contours succeeds.";
  let transforms =
    W.Transforms.empty
    |> W.Transforms.add_symmetry_X ~r0:(Uchar.of_char 'X')
    |> W.Transforms.add_symmetry_X ~r0:(Uchar.of_char ' ')
  in
  Array.map (Array.map Uchar.of_char)
    [|
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'X' |];
      [| 'X'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; 'X'; 'X'; 'X'; 'X'; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; 'X'; 'X'; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'X'; 'X' |];
      [| 'X'; 'X'; 'X'; 'X'; ' '; ' '; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; 'X'; 'X'; ' '; ' '; ' ' |];
      [| ' '; ' '; ' '; ' '; ' '; ' '; 'X'; 'X'; ' ' |];
    |]
  |> W.parse_matrix_overlap ~transforms 3
  |> W.Overlapping.make output |> run W.Overlapping.collapse;
  print_matrix output

let () =
  print_newline ();
  print_endline
    "Pathologic test 1: input with a single tile succeeds (simple tiled).";
  Array.map (Array.map Uchar.of_char) [| [| 'x'; 'x' |]; [| 'x'; 'x' |] |]
  |> W.parse_matrix_simple |> W.Simple.make output |> run W.Simple.collapse;
  print_matrix output

let () =
  print_newline ();
  print_endline
    "Pathologic test 2: input with a single tile succeeds (overlapping).";
  Array.map (Array.map Uchar.of_char) [| [| 'x'; 'x' |]; [| 'x'; 'x' |] |]
  |> W.parse_matrix_overlap 3 |> W.Overlapping.make output
  |> run W.Overlapping.collapse;
  print_matrix output

let fail_success s = "Failed successfully: " ^ s
let fail_failure _ = "Failed unsuccessfuly."

let () =
  print_newline ();
  print_endline "Error test 1: non-rectangular input is an error.";
  print_endline
  @@
  try
    [| [| ' '; ' ' |]; [| ' '; ' '; ' ' |] |]
    |> Array.map (Array.map Uchar.of_char)
    |> W.parse_matrix_simple |> fail_failure
  with Invalid_argument s -> fail_success s

let () =
  print_newline ();
  print_endline "Error test 2: empty input is an error.";
  print_endline
  @@
  try
    [| [||] |] |> W.parse_matrix_simple |> W.Simple.make output |> fail_failure
  with Invalid_argument s -> fail_success s

let () =
  print_newline ();
  print_endline "Error test 3: small overlap size is an error.";
  print_endline
  @@
  try
    Array.map (Array.map Uchar.of_char) [| [| 'x'; 'x' |]; [| 'x'; 'x' |] |]
    |> W.parse_matrix_overlap 1 |> W.Overlapping.make output |> fail_failure
  with Invalid_argument s -> fail_success s

let () =
  print_newline ();
  print_endline "Error test 4: a duplicate in a transforms map is an error.";
  print_endline
  @@
  try
    W.Transforms.(
      empty
      |> add_symmetry_X ~r0:(Uchar.of_char 'X')
      |> add_symmetry_slash ~r0:(Uchar.of_char '/') ~r90:(Uchar.of_char 'X'))
    |> fail_failure
  with Failure s -> fail_success s
