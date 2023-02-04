(lang dune 3.6)

(name wave_function_collapse)

(generate_opam_files true)

(source
 (github johnridesabike/wave-function-collapse))

(authors "John Jackson")

(maintainers "John Jackson")

(license MPL-2.0)

(package
 (name wave_function_collapse)
 (synopsis "The wave function collapse algorithm")
 (description "A demonstration of the wave function collapse algorithm, including the simple tiled model and the overlapping model.")
 (depends ocaml dune)
 (tags
  ("wave function collapse" algorithm)))
