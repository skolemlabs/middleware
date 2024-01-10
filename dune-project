(lang dune 3.11)

(name middleware)
(version 0.0.1)

(generate_opam_files true)

(source
 (github skolemlabs/midddleware))

(authors "Trent Small <sixstring982@gmail.com>")

(maintainers "Trent Small <sixstring982@gmail.com>")

(license MIT)

(package
 (name middleware)
 (synopsis "Composable stacked functions, which can respond to inner calls")
 (depends ocaml dune)
 (tags
  (middleware composition funciton)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
