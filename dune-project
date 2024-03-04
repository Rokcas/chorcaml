(lang dune 3.14)

(name chorcaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name chorcaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (lwt :base-unix) (alcotest :with-test) (alcotest-lwt :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
