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
(** Our tiles are Unicode characters. *)

exception Contradiction of { y : int; x : int }
(** This is raised if the wave function collapse algorithm encounters a
    contradiction. The [y] and [x] indicate the coordinates where the
    contradiction occurred.

    It is always possible that a random run of the algorithm will raise this
    error, although some inputs make it more likely than others.

    A simple way to handle this error is to just re-run the algorithm. *)

module Transforms : sig
  (** The algorithm has no knowledge of which [Uchar.t] tiles are rotations or
      reflections of each other. (Characters are just integers.) In order to
      allow rotations and reflections of tiles, we need to build a map with that
      information. *)

  type t

  val empty : t

  val add :
    r0:tile ->
    ?r90:tile ->
    ?r180:tile ->
    ?r270:tile ->
    ?r0_reflect:tile ->
    ?r90_reflect:tile ->
    ?r180_reflect:tile ->
    ?r270_reflect:tile ->
    t ->
    t
  (** Add a tile along with all of its rotations and reflections. Reflections
      happen on the X-axis, so the right and left sides switch. You should
      specify all the possible rotations and reflections, even if they seem
      redundant because a tile is symmetrical.

      Usually a tile has certain symmetric properties, in which case it's
      simpler to use one of the helper functions below.

      @raise Failure if any of the tiles are already in the map. *)

  val add_symmetry_X : r0:tile -> t -> t
  (** A shortcut for {!add} for a tile with the symmetric properties of the
      character [X]. *)

  val add_symmetry_T : r0:tile -> r90:tile -> r180:tile -> r270:tile -> t -> t
  (** A shortcut for {!add} for a tile with the symmetric properties of the
      character [T]. *)

  val add_symmetry_I : r0:tile -> r90:tile -> t -> t
  (** A shortcut for {!add} for a tile with the symmetric properties of the
      character [I]. *)

  val add_symmetry_L : r0:tile -> r90:tile -> r180:tile -> r270:tile -> t -> t
  (** A shortcut for {!add} for a tile with the symmetric properties of the
      character [L]. *)

  val add_symmetry_slash : r0:tile -> r90:tile -> t -> t
  (** A shortcut for {!add} for a tile with the symmetric properties of the
      character [/]. *)

  val remove : tile -> t -> t
  (** Remove a given tile along with all of its rotations and reflections. *)
end

(** This is the common interface shared among both models. *)
module type S = sig
  type t
  (** The algorithm's internal state. *)

  val collapse : t -> t option
  (** Collapse a random point in the wave function and propagate the
      consequences. This writes changes to the state's given 2-D array and
      returns an updated state value. If no further collapse is possible, it
      returns [None]. *)

  type data
  (** Parsed input data. This contains tile compatibilities and tile weights. *)

  val make : tile array array -> data -> t
  (** Create a new state for the algorithm. It will write its output to the
      given 2-D array.

      @raise Invalid_argument if the input data is empty. *)
end

module Simple : S
(** This is the "simple tiled model" version of the algorithm. *)

val parse_matrix_simple :
  ?transforms:Transforms.t -> tile array array -> Simple.data
(** Parse a 2-D array matrix using the simple tiled model.

    @raise Invalid_argument if the input does not have rectangular
    dimensions. *)

module Overlapping : S
(** This is the "overlapping model" version of the algorithm. *)

val parse_matrix_overlap :
  ?transforms:Transforms.t -> int -> tile array array -> Overlapping.data
(** Parse a 2-D array matrix using the overlapping model. The integer argument
    specifies how large the overlap is.

    @raise Invalid_argument if the input does not have rectangular
    dimensions or if the overlap size is less than 2. *)
