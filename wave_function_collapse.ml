(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type tile = Uchar.t

module Coordinates : sig
  (** These are your basic X-Y coordinates. We put them in a module so that
      they're easy to use as map keys. (In our matrix, we use the Y as the first
      dimension and the X second.)

      Throughout the rest of the module, we look-up values with this type rather
      than regular [int] values. *)

  type t = { y : int; x : int }

  val compare : t -> t -> int
end = struct
  type t = { y : int; x : int }

  let compare { y; x } b =
    match Int.compare y b.y with 0 -> Int.compare x b.x | i -> i
end

exception Contradiction of { y : int; x : int }

let raise_contradiction Coordinates.{ y; x } = raise @@ Contradiction { y; x }

module Output : sig
  (** The wave function collapse algorithm will write its output onto a
      2-dimensional array. *)

  type t = tile array array

  val height : t -> int
  val width : t -> int
  val set : t -> Coordinates.t -> tile -> unit
end = struct
  type t = tile array array

  let height = Array.length
  let width a = Array.length a.(0)
  let set a Coordinates.{ y; x } v = a.(y).(x) <- v
end

module Direction : sig
  (** This helps us navigate around the matrix and as well as track which tiles
      are compatible with each other. *)

  type t = private Up | Down | Left | Right

  val fold_valid :
    height:int ->
    width:int ->
    Coordinates.t ->
    (t -> Coordinates.t -> 'acc -> 'acc) ->
    'acc ->
    'acc
  (** Given a height and width, this folds over the valid directions from the
      given coordinates and ensures that we never step outside of the matrix's
      edges. *)

  val rotate : t -> t
  (** {v
    U        L
  L   R -> D   U
    D        R
v} *)

  val reflect : t -> t
  (** Reflections only apply to the X axis.
     {v
    U        U
  L   R -> R   L
    D        D
v} *)
end = struct
  type t = Up | Down | Left | Right

  open Coordinates

  let fold_valid ~height ~width { y; x } f acc =
    let acc = if y > 0 then f Up { y = pred y; x } acc else acc in
    let acc = if x < pred width then f Right { y; x = succ x } acc else acc in
    let acc = if y < pred height then f Down { y = succ y; x } acc else acc in
    let acc = if x > 0 then f Left { y; x = pred x } acc else acc in
    acc

  let rotate x =
    match x with Up -> Right | Right -> Down | Down -> Left | Left -> Up

  let reflect x =
    match x with Up -> Up | Right -> Left | Down -> Down | Left -> Right
end

module CompatibilityOracle : sig
  (** This module is only used for the simple tiled model. *)

  type t
  (** This is responsible for telling us which combinations of tiles and
      directions are compatible. *)

  val empty : t
  val add : tile -> Direction.t -> tile -> t -> t
  val check : tile -> Direction.t -> tile -> t -> bool
end = struct
  (** There are multiple ways to implement this. The simplest would be to use a
      set of [(Tile.t * Direction.t * Tile.t)] 3-tuples. This version makes
      look-ups a bit more efficient.

      First, it simplifies the directions into only two: up-to-down and
      left-to-right. Tiles are sorted accordingly, so [a Down b] and [b Up a]
      are equivalent. Second, the first tile is stored as a key in a map
      associated with a set that contains the second tile.

      This saves space and doesn't require us to allocate a new tuple for every
      look-up. *)

  module TileMap = Map.Make (Uchar)
  module TileSet = Set.Make (Uchar)

  type t = {
    up_to_down : TileSet.t TileMap.t;
    left_to_right : TileSet.t TileMap.t;
  }

  let empty = { up_to_down = TileMap.empty; left_to_right = TileMap.empty }

  open Direction

  let add_aux first second map =
    TileMap.update first
      (function
        | None -> Some (TileSet.singleton second)
        | Some s -> Some (TileSet.add second s))
      map

  let add current direction other t =
    match direction with
    | Up -> { t with up_to_down = add_aux other current t.up_to_down }
    | Down -> { t with up_to_down = add_aux current other t.up_to_down }
    | Left -> { t with left_to_right = add_aux other current t.left_to_right }
    | Right -> { t with left_to_right = add_aux current other t.left_to_right }

  let check_aux first second map =
    try TileMap.find first map |> TileSet.mem second with Not_found -> false

  let check current direction other { up_to_down; left_to_right } =
    match direction with
    | Up -> check_aux other current up_to_down
    | Down -> check_aux current other up_to_down
    | Left -> check_aux other current left_to_right
    | Right -> check_aux current other left_to_right
end

module type PATTERN = sig
  (** This is a specification for the modules we need to build the different
      models of the algorithm. *)

  type t

  val to_tile : t -> tile
  val equal : t -> t -> bool

  val check : t -> Direction.t -> t -> CompatibilityOracle.t -> bool
  (** Only the simple tiled model needs the compatibility oracle, but we have to
     require it in the interface for both for consistency. The overlapping model
     just ignores it. *)
end

module SimplePattern : PATTERN with type t = tile = struct
  type t = tile
  (** For the simple tiled model, our patterns are just tiles and we use the
      compatibility oracle to check for compatibility. *)

  let to_tile = Fun.id
  let equal = Uchar.equal
  let check = CompatibilityOracle.check
end

module OverlappingPattern : PATTERN with type t = tile array array = struct
  type t = tile array array
  (** For the overlapping model, we use NxN matrices of tiles and determine
      compatibility by checking if they overlap by N-1xN or NxN-1. *)

  (** When we render a pattern to the output we only use the top-left tile. *)
  let to_tile a = a.(0).(0)

  let equal_cols a b = Array.for_all2 Uchar.equal a b
  let equal a b = Array.for_all2 equal_cols a b

  let overlap f first second =
    let n = Array.length first in
    let rec loop i =
      if i = n then true
      else if f first.(i) second.(pred i) then loop (succ i)
      else false
    in
    loop 1

  let overlap_up_down a b = overlap equal_cols a b
  let overlap_cols a b = overlap Uchar.equal a b
  let overlap_left_right a b = Array.for_all2 overlap_cols a b

  let check current direction other _ =
    match direction with
    | Direction.Up -> overlap_up_down other current
    | Direction.Down -> overlap_up_down current other
    | Direction.Left -> overlap_left_right other current
    | Direction.Right -> overlap_left_right current other
end

(** The algorithm works the same for both the simple tiled model and the
    overlapping model. The only differences are how each one represents its
    patterns of tiles and how each one checks for compatibility between
    patterns. We can easily parameterize these with a functor. *)
module MakeAlgorithm (Pattern : PATTERN) = struct
  module Coefficient : sig
    type t
    (** A set of possible [Pattern.t] values mapped to their [float] weighted
        probabilities. *)

    val empty : t
    val is_empty : t -> bool
    val singleton : Pattern.t -> t

    val shannon_entropy : t -> float
    (** The Shannon entropy for the given coefficient. *)

    val update_weight : (float -> float) -> Pattern.t -> t -> t
    (** [update_weight opt f t] applies function [f] to the weight of pattern
        [opt] and replaces the weight with the result. If the new weight is
        less than or equal to [0.0] then the pattern is removed.

        If the given pattern does not exist in the coefficient yet, then it is
        implicitly added with a weight of [0.0] before applying the function. *)

    val choose_random : t -> Pattern.t
    (** Choose a randomly-selected pattern. Patterns with a higher weight are
        more likely to be chosen. *)

    (** When there's only one possible pattern, we collapse and remove it from
        the algorithm. This type tracks that for us. *)
    type maybe_collapsed = private Collapsed of Pattern.t | Coefficient of t

    val filter_possible :
      CompatibilityOracle.t -> t -> Direction.t -> t -> maybe_collapsed option
    (** [filter_possible oracle a dir b] returns an updated [b] with all of the
        impossible patterns removed. Returns [None] if nothing was changed.

        @raise Failure if the removals create an empty coefficient. *)
  end = struct
    type t = {
      opts : (Pattern.t * float) list;
          (** This is a map of patterns to their weight values. It's slightly
              more efficient to use an association list instead of a regular map
              because we mostly don't look-up patterns, only fold over them. *)
      entropy : float;
    }

    let empty = { opts = []; entropy = 0.0 }
    let is_empty = function { opts = []; _ } -> true | _ -> false
    let singleton opt = { opts = [ (opt, 1.0) ]; entropy = 0.0 }
    let shannon_entropy t = t.entropy

    let calc_shannon_entropy opts =
      let weight_sum, weight_log_sum =
        List.fold_left
          (fun (weight_sum, weight_log_sum) (_, weight) ->
            ( weight_sum +. weight,
              weight_log_sum +. (weight *. Float.log2 weight) ))
          (0.0, 0.0) opts
      in
      let entropy = Float.log2 weight_sum -. (weight_log_sum /. weight_sum) in
      { opts; entropy }

    let rec update_loop f opt = function
      | [] ->
          let weight = f 0.0 in
          if weight > 0.0 then [ (opt, weight) ] else []
      | (opt', weight) :: tl when Pattern.equal opt opt' ->
          let weight = f weight in
          if weight > 0.0 then (opt', weight) :: tl else tl
      | hd :: tl -> hd :: update_loop f opt tl

    let update_weight f opt { opts; _ } =
      update_loop f opt opts |> calc_shannon_entropy

    let choose_random { opts; _ } =
      let rnd =
        List.fold_left (fun acc (_, weight) -> weight +. acc) 0.0 opts
        |> Random.float
      in
      let rec loop rnd = function
        | [] -> assert false
        | (opt, weight) :: tl ->
            let rnd = rnd -. weight in
            if rnd < 0.0 then opt else loop rnd tl
      in
      loop rnd opts

    type maybe_collapsed = Collapsed of Pattern.t | Coefficient of t

    let filter_possible compatibility_oracle base dir other =
      let new_opts =
        List.filter
          (fun (other_opt, _) ->
            List.exists
              (fun (base_opt, _) ->
                Pattern.check base_opt dir other_opt compatibility_oracle)
              base.opts)
          other.opts
      in
      if List.compare_lengths new_opts other.opts = 0 then None
      else
        match new_opts with
        | [] -> failwith "Coefficient.filter_possible"
        | [ (opt, _) ] -> Some (Collapsed opt)
        | new_opts -> Some (Coefficient (calc_shannon_entropy new_opts))
  end

  module WaveFunction : sig
    type t
    (** This tracks which coordinates contain coefficients and which have
        collapsed into a single tile.

        Even though it mutates its given [Output.t] as each coefficient
        collapses, all other properties are updated functionally without
        mutation. *)

    val height : t -> int
    (** The output's height. *)

    val width : t -> int
    (** The output's width. *)

    val make : Output.t -> Coefficient.t -> t
    val find : Coordinates.t -> t -> Coefficient.t

    val collapse_random : Coordinates.t -> Coefficient.t -> t -> t
    (** Collapse the wave function at the given coordinates to a single, definite
        pattern chosen by [Coefficient.choose_random]. *)

    val update : Coordinates.t -> Coefficient.maybe_collapsed -> t -> t
    (** Update the given coordinates with a new coefficient, which may be
        collapsed.

        @raise Contradiction if the constraint is not possible. *)

    val min_entropy : t -> (Coordinates.t * Coefficient.t * float) option
    (** Returns the coordinates, the coefficient, and the entropy of the location
        whose coefficient has the lowest entropy.

        If all patterns have collapsed, then this returns [None]. *)
  end = struct
    module CoordMap = Map.Make (Coordinates)

    type t = {
      height : int;
      width : int;
      output : Output.t;
      coefficients : Coefficient.t CoordMap.t;
          (** This is a map of [Coordinate.t] to [Coefficient.t]. It uses a
              map that shrinks as the algorithm progresses. *)
      collapsed : Coefficient.t CoordMap.t;
          (** A store of the collapsed coefficients. Each is a singleton. *)
    }

    let height t = t.height
    let width t = t.width

    let make_coefficient_map ~height ~width coefficient =
      let tbl = ref CoordMap.empty in
      for y = 0 to pred height do
        for x = 0 to pred width do
          tbl := CoordMap.add Coordinates.{ y; x } coefficient !tbl
        done
      done;
      !tbl

    let make output coefficient =
      let height, width = Output.(height output, width output) in
      let coefficients = make_coefficient_map ~height ~width coefficient in
      { height; width; output; coefficients; collapsed = CoordMap.empty }

    let find coords { coefficients; collapsed; _ } =
      try CoordMap.find coords coefficients
      with Not_found -> CoordMap.find coords collapsed

    let collapse coords opt t =
      Output.set t.output coords (Pattern.to_tile opt);
      {
        t with
        coefficients = CoordMap.remove coords t.coefficients;
        collapsed = CoordMap.add coords (Coefficient.singleton opt) t.collapsed;
      }

    let collapse_random coords opts t =
      collapse coords (Coefficient.choose_random opts) t

    let update coords opt t =
      match opt with
      | Coefficient.Collapsed opt -> collapse coords opt t
      | Coefficient.Coefficient opts ->
          { t with coefficients = CoordMap.add coords opts t.coefficients }

    let min_entropy { coefficients; _ } =
      CoordMap.fold
        (fun coords opts acc ->
          (* Add some random noise to mix things up a little. *)
          let entropy =
            Coefficient.shannon_entropy opts -. Random.float 0.001
          in
          match acc with
          | None -> Some (coords, opts, entropy)
          | Some (_, _, min_entropy) ->
              if entropy < min_entropy then Some (coords, opts, entropy)
              else acc)
        coefficients None
  end

  type t = {
    wave : WaveFunction.t;
    compatibility_oracle : CompatibilityOracle.t;
  }

  (** Now that all of our types are defined, we can orchestrate the
      wave function collapse algorithm. *)

  (** Propagate the consequences of the wave function collapse at a given list
      of coordinates. When the wave function at a point collapses to a fixed
      pattern (tile), then some patterns may no longer be possible at its
      surrounding coordinates.

      This keeps propagating the consequences of the consequences repeatedly
      until no consequences remain. *)
  let rec propagate compatibility_oracle wave = function
    | [] -> wave
    | coords :: stack ->
        (* Get the set of all possible patterns at the current location *)
        let cur_possible_opts = WaveFunction.find coords wave in
        (* Fold through each location immediately adjacent to the current
           location and update the wave and stack. *)
        let height, width = WaveFunction.(height wave, width wave) in
        let wave, stack =
          Direction.fold_valid ~height ~width coords
            (fun dir other_coords (wave, stack) ->
              let other_possible_opts = WaveFunction.find other_coords wave in
              (* Remove any impossible patterns from the adjacent location. *)
              match
                Coefficient.filter_possible compatibility_oracle
                  cur_possible_opts dir other_possible_opts
              with
              | None -> (wave, stack) (* Nothing was changed. *)
              | Some opts ->
                  (* If the coefficient was updated, then add its coordinates to
                     the stack to further propagate the changes. *)
                  ( WaveFunction.update other_coords opts wave,
                    other_coords :: stack )
              | exception Failure _ -> raise_contradiction other_coords)
            (wave, stack)
        in
        propagate compatibility_oracle wave stack

  (** 1. Find the coordinates with minimum entropy.
      2. Collapse the wave function at these coordinates.
      3. Propagate the consequences of this collapse. *)
  let collapse { wave; compatibility_oracle } =
    match WaveFunction.min_entropy wave with
    | Some (coords, opts, _entropy) ->
        let wave = WaveFunction.collapse_random coords opts wave in
        let wave = propagate compatibility_oracle wave [ coords ] in
        Some { wave; compatibility_oracle }
    | None -> None
end

(** This concludes the wave function collapse algorithm. The remainder of this
    code is to facilitate running the algorithm for the examples. *)

module Transforms = struct
  (** This helps the parse functions rotate and reflect patterns. See the
      interface for more information on why it's necessary. *)

  (** Here's a visualization of the transforms. Reflections apply to the X axis.

                    0 deg     90 deg     180 deg    270 deg
                  =========  =========  =========  =========
                  |   U   |  |   L   |  |   D   |  |   R   |
          normal: | L + R |  | D + U |  | R + L |  | U + D |
                  |   D   |  |   R   |  |   U   |  |   L   |
                  ---------  ---------  ---------  ---------
                  |   U   |  |   L   |  |   D   |  |   R   |
      reflection: | R + L |  | U + D |  | L + R |  | D + U |
                  |   D   |  |   R   |  |   U   |  |   L   |
                  =========  =========  =========  ========= *)

  (** We have two concerns regarding transforms. First, we need to track all of
      the possible rotations and reflections of any given tile (or matrix of
      tiles). Second, we need to track a set of unique tiles (or matrices).
      Sometimes we want to access every combination of rotations and
      reflections, and sometimes we only want the unique ones.

      For example, we use [fold] to add all of the rotations and reflections to
      a coefficient. We need to use a set because otherwise tiles that are
      symmetrical on every side, such as [' '], will have eight times the weight
      of a tile with no symmetry. *)

  module TileSet = Set.Make (Uchar)

  type rotations = {
    r0 : tile option;
    r90 : tile option;
    r180 : tile option;
    r270 : tile option;
  }

  type transforms = { normal : rotations; reflect : rotations; set : TileSet.t }

  module TileMap = Map.Make (Uchar)

  type t = transforms TileMap.t
  (** A map of each tile to each of its rotations and reflections.

      Invariant: every tile in every value must also be a key in the map to a
      [transforms] value that's rotated or reflected. *)

  let empty = TileMap.empty

  (** Fold through all reflections and rotations of a tile. *)
  let fold f a m init =
    match TileMap.find a m with
    | exception Not_found -> f a init
    | { set; _ } -> TileSet.fold f set init

  let rotate_matrix arr =
    let height, width = (Array.length arr, Array.length arr.(0)) in
    let result = Array.make_matrix height width Uchar.min in
    for y = 0 to pred height do
      for x = 0 to pred width do
        result.(x).(pred height - y) <- arr.(y).(x)
      done
    done;
    result

  let reflect_matrix arr =
    let height, width = (Array.length arr, Array.length arr.(0)) in
    let result = Array.make_matrix height width Uchar.min in
    for y = 0 to pred height do
      for x = 0 to pred width do
        result.(y).(pred width - x) <- arr.(y).(x)
      done
    done;
    result

  (** Fold through all reflections and rotations of a 2-D array of tiles. *)
  let fold_matrix f mtrx_0 m =
    let mtrx_90 = rotate_matrix mtrx_0 in
    let mtrx_180 = rotate_matrix mtrx_90 in
    let mtrx_270 = rotate_matrix mtrx_180 in
    (* Use a set to filter duplicates and invalid transformations.
       A list is simpler than a regular set here. *)
    let set_add f a l =
      match
        Array.map (Array.map (fun x -> TileMap.find x m |> f |> Option.get)) a
      with
      | x -> if List.mem x l then l else x :: l
      | exception Not_found -> l
    in
    [ mtrx_0 ]
    |> set_add (fun x -> x.normal.r90) mtrx_90
    |> set_add (fun x -> x.normal.r180) mtrx_180
    |> set_add (fun x -> x.normal.r270) mtrx_270
    |> set_add (fun x -> x.reflect.r0) (reflect_matrix mtrx_0)
    |> set_add (fun x -> x.reflect.r90) (reflect_matrix mtrx_90)
    |> set_add (fun x -> x.reflect.r180) (reflect_matrix mtrx_180)
    |> set_add (fun x -> x.reflect.r270) (reflect_matrix mtrx_270)
    |> List.fold_right f

  let fold2_opts f a dir b acc =
    match (a, b) with Some a, Some b -> f a dir b acc | _ -> acc

  let fold2_rotations f { r0; r90; r180; r270 } dir0 dir90 dir180 dir270 b acc =
    fold2_opts f r0 dir0 b.r0 acc
    |> fold2_opts f r90 dir90 b.r90
    |> fold2_opts f r180 dir180 b.r180
    |> fold2_opts f r270 dir270 b.r270

  (** Given a tile and a direction to another tile, fold through all possible
      rotations and reflections of the two tiles and the corresponding
      directions between them. *)
  let fold2 f a dir b m init =
    match (TileMap.find a m, TileMap.find b m) with
    | exception Not_found -> f a dir b init
    | { normal; reflect; set = _ }, b ->
        let module D = Direction in
        let dir_90 = D.rotate dir in
        let dir_180 = D.rotate dir_90 in
        let dir_270 = D.rotate dir_180 in
        fold2_rotations f normal dir dir_90 dir_180 dir_270 b.normal init
        |> fold2_rotations f reflect (D.reflect dir) (D.reflect dir_90)
             (D.reflect dir_180) (D.reflect dir_270) b.reflect

  let rotate_aux { r0; r90; r180; r270 } =
    { r0 = r270; r90 = r0; r180 = r90; r270 = r180 }

  let rotate { normal; reflect; set } =
    { normal = rotate_aux normal; reflect = rotate_aux reflect; set }

  (** When we swap the reflected and non-reflected sides, we must also reverse
      them because the reflections rotate backwards. *)
  let reverse { r0; r90; r180; r270 } =
    { r0 = r270; r90 = r180; r180 = r90; r270 = r0 }

  let reflect { normal; reflect; set } =
    { normal = reverse reflect; reflect = reverse normal; set }

  let set_add x s = match x with Some x -> TileSet.add x s | None -> s

  let map_add x m =
    match x.normal.r0 with Some r0 -> TileMap.add r0 x m | None -> m

  let add_fail tile =
    failwith ("Tile " ^ string_of_int (Uchar.to_int tile) ^ " is already set.")

  let add ~r0 ?r90 ?r180 ?r270 ?r0_reflect ?r90_reflect ?r180_reflect
      ?r270_reflect m =
    let a =
      {
        normal = { r0 = Some r0; r90; r180; r270 };
        reflect =
          {
            r0 = r0_reflect;
            r90 = r90_reflect;
            r180 = r180_reflect;
            r270 = r270_reflect;
          };
        set =
          TileSet.singleton r0 |> set_add r90 |> set_add r180 |> set_add r270
          |> set_add r0_reflect |> set_add r90_reflect |> set_add r180_reflect
          |> set_add r270_reflect;
      }
    in
    let b = rotate a in
    let c = rotate b in
    let d = rotate c in
    TileMap.singleton r0 a |> map_add b |> map_add c |> map_add d
    |> map_add (reflect a)
    |> map_add (reflect b)
    |> map_add (reflect c)
    |> map_add (reflect d)
    |> TileMap.union add_fail m

  let add_symmetry_X ~r0 m =
    add ~r0 ~r90:r0 ~r180:r0 ~r270:r0 ~r0_reflect:r0 ~r90_reflect:r0
      ~r180_reflect:r0 ~r270_reflect:r0 m

  (* ┬ ┤ ┴ ├
     ┬ ├ ┴ ┤ *)
  let add_symmetry_T ~r0 ~r90 ~r180 ~r270 m =
    add ~r0 ~r90 ~r180 ~r270 ~r0_reflect:r0 ~r90_reflect:r270 ~r180_reflect:r180
      ~r270_reflect:r90 m

  (* │ ─ │ ─
     │ ─ │ ─ *)
  let add_symmetry_I ~r0 ~r90 m =
    add ~r0 ~r90 ~r180:r0 ~r270:r90 ~r0_reflect:r0 ~r90_reflect:r90
      ~r180_reflect:r0 ~r270_reflect:r90 m

  (* └ ┌ ┐ ┘
     ┘ ┐ ┌ └ *)
  let add_symmetry_L ~r0 ~r90 ~r180 ~r270 m =
    add ~r0 ~r90 ~r180 ~r270 ~r0_reflect:r270 ~r90_reflect:r180
      ~r180_reflect:r90 ~r270_reflect:r0 m

  (* /\/\
     \/\/ *)
  let add_symmetry_slash ~r0 ~r90 m =
    add ~r0 ~r90 ~r180:r0 ~r270:r90 ~r0_reflect:r90 ~r90_reflect:r0
      ~r180_reflect:r90 ~r270_reflect:r0 m

  let remove x m =
    match TileMap.find x m with
    | exception Not_found -> m
    | { set; _ } -> TileSet.fold TileMap.remove set m
end

(** This is the common public interface shared among both models. *)
module type S = sig
  type t

  val collapse : t -> t option

  type data

  val make : Output.t -> data -> t
end

let err_nonrect = "Non-rectangular input"
let err_empty = "Empty data"

module Simple = struct
  include MakeAlgorithm (SimplePattern)

  type data = {
    coefficient : Coefficient.t;
    compatibility_oracle : CompatibilityOracle.t;
  }

  let empty_data =
    {
      coefficient = Coefficient.empty;
      compatibility_oracle = CompatibilityOracle.empty;
    }

  let make arr { coefficient; compatibility_oracle } =
    if Coefficient.is_empty coefficient then invalid_arg err_empty
    else { wave = WaveFunction.make arr coefficient; compatibility_oracle }
end

let ( .%() ) arr Coordinates.{ y; x } =
  try arr.(y).(x) with Invalid_argument _ -> invalid_arg err_nonrect

let parse_matrix_simple ?(transforms = Transforms.empty) matrix =
  (* This is not efficient, but input matrices are usually small. *)
  let open Simple in
  let height = Array.length matrix in
  match Array.length matrix.(0) with
  | exception Invalid_argument _ -> empty_data
  | width ->
      Array.to_seqi matrix
      |> Seq.flat_map (fun (y, row) ->
             if Array.length row <> width then invalid_arg err_nonrect
             else
               Array.to_seqi row
               |> Seq.map (fun (x, tile) -> (Coordinates.{ y; x }, tile)))
      |> Seq.fold_left
           (fun { coefficient; compatibility_oracle } (coords, current_tile) ->
             let coefficient =
               Transforms.fold
                 (Coefficient.update_weight (Float.add 1.0))
                 current_tile transforms coefficient
             in
             let compatibility_oracle =
               Direction.fold_valid ~width ~height coords
                 (fun dir coords compatibility_oracle ->
                   let other_tile = matrix.%(coords) in
                   Transforms.fold2 CompatibilityOracle.add current_tile dir
                     other_tile transforms compatibility_oracle)
                 compatibility_oracle
             in
             { coefficient; compatibility_oracle })
           empty_data

module Overlapping = struct
  include MakeAlgorithm (OverlappingPattern)

  type data = Coefficient.t

  let empty_data = Coefficient.empty

  let make arr coefficient =
    if Coefficient.is_empty coefficient then invalid_arg err_empty
    else
      {
        wave = WaveFunction.make arr coefficient;
        compatibility_oracle = CompatibilityOracle.empty;
      }
end

(** We wrap around the edges so patterns repeat in every direction. *)
let rec get_wrap i a =
  let len = Array.length a in
  if len = 0 then invalid_arg err_nonrect
  else if i < len then a.(i)
  else get_wrap (i - len) a

let make_pattern size Coordinates.{ y; x } arr =
  Array.init size @@ fun y' ->
  Array.init size @@ fun x' -> get_wrap (y + y') arr |> get_wrap (x + x')

let parse_matrix_overlap ?(transforms = Transforms.empty) pattern_size matrix =
  (* This is not efficient, but input matrices are usually small. *)
  let open Overlapping in
  if pattern_size < 2 then
    invalid_arg "Overlap pattern size must be 2 or greater.";
  match Array.length matrix.(0) with
  | exception Invalid_argument _ -> empty_data
  | width ->
      Array.to_seqi matrix
      |> Seq.flat_map (fun (y, row) ->
             if Array.length row <> width then invalid_arg err_nonrect
             else
               Array.to_seqi row |> Seq.map (fun (x, _) -> Coordinates.{ y; x }))
      |> Seq.fold_left
           (fun coefficient coords ->
             Transforms.fold_matrix
               (Coefficient.update_weight (Float.add 1.0))
               (make_pattern pattern_size coords matrix)
               transforms coefficient)
           empty_data
