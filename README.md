# Wave function collapse algorithm in OCaml

This is an OCaml implementation of the wave function collapse (WFC) algorithm.
It takes an input image and procedurally generates a new image that resembles
the input. The beauty of the WFC algorithm is how simple it is. With just a few
basic rules, it can magically generate images without anything fancy like neural
networks.

Images are only one common use-case. You can use it to generate maps, seating
charts, or text files such as the examples in this repository.

    |      Input      |                     Output                     |
    |-----------------|------------------------------------------------|
    |                 |      │  │  │  │ │ ╰─┬─╯         │  │ │  ╰──╮ │ |
    |                 |      ├──╯  │  │ │   │           │  ╰─┤     │ ╰─|
    |                 |      │     │  │ │   │           │    │     │   |
    |                 |      ╰─────╯  │ ├─╮ │  ╭─────╮  ├──╮ ├─╮   ├──╮|
    |                 |╭──╮           │ │ │ │  │     │  │  │ │ │   │  │|
    |         │       |│  │   ╭───┬─╮ ╰─╯ ├─┼──╯  ╭──╯  │  │ │ ╰───╯  │|
    |     ─╮  ├─      |│  ├─╮ │   │ │     │ │     │     │  │ │      ╭─╯|
    |      │  │       |│  │ │ ╰─╮ ╰─┼─┬───╯ │     │     │  │ │ ╭──╮ │  |
    |     ─┼──┼─      |╰──╯ │   │   │ │     │     ╰─────┼──┤ │ │  │ ╰──|
    |      │  │       |     │   ├─┬─┤ ├─╮   │           │  │ │ ╰──╯    |
    |      ╰──┤       |     ╰─╮ │ │ │ │ │ ╭─┴─╮         │  │ │         |
    |                 |       │ ├─╯ ╰─┼─┤ │   │         │  ╰─┼──┬──────|
    |                 |┬──────┤ │     │ │ ├───╯         ╰─╮  │  │      |
    |                 |│      │ ├─╮   ╰─┤ │    ╭─┬──┬──╮  │  │  ├─╮    |
    |                 |├─╮    │ │ │     │ ├──╮ │ │  │  │  │  │  │ │    |
    |                 |│ │    ╰─╯ │     │ │  │ │ │  ╰──┼──┴──┼──┤ │    |

## How it works

The wave function collapse algorithm's design comes from the quantum mechanics
concept of the same name. When the algorithm begins, it initializes each point
on the output as a "wave," or a set of possible values. With each iteration, the
algorithm chooses a point and "collapses" it into a single value. Each potential
value has a weighted probability of being chosen, and there are rules that
govern which values may appear near each other. Every collapse sets off a chain
reaction, propagating more collapses throughout the output.

Once you see it in action, it's usually easy to intuit how it works even without
knowing the formal rules. In fact, it's similar to how us humans solve things
like sudoku puzzles. [This blog post explains it in more detail][0].

[0]: https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/

## This project

This OCaml implementation was originally derived from [this Python version][1]
(created by the author of the above blog post), but has been expanded to include
more features. It also takes much inspiration from [this C# version][2].

[1]: https://github.com/robert/wavefunction-collapse
[2]: https://github.com/mxgmn/WaveFunctionCollapse

This version, like the Python one, should be hopefully easy to understand and
learn from, although the Python one is simpler. The algorithm resides in a
single module, `wave_function_collapse.ml[i]` and contains lots of comments. It
could serve as a foundation for building a more sophisticated WFC
implementation.

Compared to the Python one, this OCaml version uses persistent data structures
and pure functions instead of imperative loops.

### The two models

This implements two versions of the WFC algorithm:

- **The simple tiled model** calculates each point on the output based on its
  immediate neighbors.
- **The overlapping model** calculates each point on the output based on an N×N
  grid around it, usually 2×2 or 3×3. The larger the value of N, the more
  closely the output will resemble the input.

Each version has it strengths and drawbacks, and each can be better suited for
certain types of inputs.

## Usage

In addition to the algorithm itself, this repository includes code for an
executable that reads text from stdin and writes generated text to stdout.

To run this code, you need an OCaml development environment that uses the Dune
build system. [See this guide for getting up-and-running with OCaml][3].

[3]: https://ocaml.org/docs/up-and-running

You can execute the examples through Dune with a command like this:

    dune exec ./bin.exe < example_pipes.txt

It uses the simple tiled model by default. To enable the overlapping model, use
the `--overlap=N` argument, where `N` is the size of the overlap. Prefer 2 or 3.

    dune exec ./bin.exe -- --overlap=3 < example_cow.txt

To see a full list of options for the examples, run this:

    dune exec ./bin.exe -- --help

## Examples

These are the result of a few runs with the text files in this repository. Some
of the example files are better suited for the simple tiled model (the default),
and some are better with the overlapping model.

    |      Input      |                     Output                     |
    |-----------------|------------------------------------------------|
    |                 |                                                |
    |                 |                                                |
    |                 |   .%%,                                         |
    |                 |.%%%~~%,                                        |
    |                 |%~~~~~~%, .%,                                  .|
    |                 |~~~~~~~~%%%~%,                       .%,      .%|
    |      .%%,       |~~~~~~~~~~~~~%,                     .%~%, .%%%%~|
    |     .%~~%,      |~~~~~~~~~~~~~~%,                   .%~~~%%%~~~~~|
    |    %%~~~~%%     |~~~~~~~~~~~~~~~%,                 .%~~~~~~~~~~~~|
    |    ~~~~~~~~     |~~~~~~~~~~~~~~~~%,          .%%, .%~~~~~~~~~~~~~|
    |    ~~~~~~~~     |~~~~~~~~~~~~~~~~~%,        .%~~%%%~~~~~~~~~~~~~~|
    |                 |~~~~~~~~~~~~~~~~~~%,      .%~~~~~~~~~~~~~~~~~~~~|
    |                 |~~~~~~~~~~~~~~~~~~~%%%,  .%~~~~~~~~~~~~~~~~~~~~~|
    |                 |~~~~~~~~~~~~~~~~~~~~~~%%%%~~~~~~~~~~~~~~~~~~~~~~|
    |                 |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    |                 |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    |-----------------|------------------------------------------------|
    |                 |(oo)\_____               ^__^                   |
    |                 |(__)\     )\/\           (oo)\_____             |
    |                 |    ||--w |              (__)\     )\/\         |
    |                 |    ||   ||                  ||--w |            |
    |                 |                             ||   ||            |
    |                 |                                              ^_|
    |  ^__^           |               ^__^                           (o|
    |  (oo)\_____     |               (oo)\_____                     (_|
    |  (__)\     )\/\ |               (__)\     )\/\                   |
    |      ||--w |    |                   ||--w |                      |
    |      ||   ||    |_                  ||   ||     ^__^             |
    |                 | )\/\                          (oo)\_____       |
    |                 | |                             (__)\     )\/\   |
    |                 |||             ^__^                ||--w |      |
    |                 |               (oo)\_____          ||   ||      |
    |                 |               (__)\     )\/\                   |
    |-----------------|------------------------------------------------|
    |                 |      │  │  │  │ │ ╰─┬─╯         │  │ │  ╰──╮ │ |
    |                 |      ├──╯  │  │ │   │           │  ╰─┤     │ ╰─|
    |                 |      │     │  │ │   │           │    │     │   |
    |                 |      ╰─────╯  │ ├─╮ │  ╭─────╮  ├──╮ ├─╮   ├──╮|
    |                 |╭──╮           │ │ │ │  │     │  │  │ │ │   │  │|
    |         │       |│  │   ╭───┬─╮ ╰─╯ ├─┼──╯  ╭──╯  │  │ │ ╰───╯  │|
    |     ─╮  ├─      |│  ├─╮ │   │ │     │ │     │     │  │ │      ╭─╯|
    |      │  │       |│  │ │ ╰─╮ ╰─┼─┬───╯ │     │     │  │ │ ╭──╮ │  |
    |     ─┼──┼─      |╰──╯ │   │   │ │     │     ╰─────┼──┤ │ │  │ ╰──|
    |      │  │       |     │   ├─┬─┤ ├─╮   │           │  │ │ ╰──╯    |
    |      ╰──┤       |     ╰─╮ │ │ │ │ │ ╭─┴─╮         │  │ │         |
    |                 |       │ ├─╯ ╰─┼─┤ │   │         │  ╰─┼──┬──────|
    |                 |┬──────┤ │     │ │ ├───╯         ╰─╮  │  │      |
    |                 |│      │ ├─╮   ╰─┤ │    ╭─┬──┬──╮  │  │  ├─╮    |
    |                 |├─╮    │ │ │     │ ├──╮ │ │  │  │  │  │  │ │    |
    |                 |│ │    ╰─╯ │     │ │  │ │ │  ╰──┼──┴──┼──┤ │    |
    |-----------------|------------------------------------------------|
    |                 |  X  X  X   X  XXX       X  X   X  XX   X  X  X |
    |                 |   XX    X  X     XX   XX   X   X       X   XX  |
    |                 |X        X   X      XXX    X     X     X        |
    |                 |X         X   XXX         X       X    X        |
    |            X    |          X      XX      X         X  X         |
    |    X            |     XX  X         X     X    XX    XX        XX|
    |     XXXX        |  XXX  XX    XXX    X   X    X  X            X  |
    |         XX      | X         XX   X   X   X   X    XX         X   |
    |           XX    |X         X      X   X   X  X      XX  XX  X    |
    |    XXXX         |    XX  XX       X   X   X   X       XX  XX    X|
    |        XX       |   X  XX          X   X   X   XX               X|
    |          XX     |  X         XXXX   X   X   X    XX              |
    |                 |XX       XXX    X   X  X   X      XX  XX  XX    |
    |                 |     XXXX        X  X   X   X       XX  XX  X   |
    |                 |   XX            X   X  X   X                XX |
    |                 |XXX       XXX   X    X  X    X                 X|

## License

    Copyright (c) 2023 John Jackson.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.
