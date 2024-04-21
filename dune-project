(lang dune 3.10)
(wrapped_executables false)

(name pynbtools)

(generate_opam_files true)

(authors "Christophe Raffalli")

(maintainers "Christophe Raffalli")

(package
 (name pynbtools)
 (synopsis "tools to work on python")
 (depends   (ocaml (>= 5.0))
  dune re ppx_rapper ppx_rapper_lwt simple_httpd caqti caqti-driver-postgresql
  ppx_rapper_lwt yojson))
