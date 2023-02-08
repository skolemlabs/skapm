(lang dune 2.5)

(name skapm)

(license Apache-2.0)
(maintainers
 "Zach Baylin <zach@skolem.tech>")
(authors
 "Zach Baylin <zach@skolem.tech>"
 "Adam Ringwood <adam.ringwood@elastic.co>"
 "Hezekiah M. Carty <hezekiah.carty@elastic.co>")

(source (github skolemlabs/skapm))

(generate_opam_files true)

(version 1.4.0)

(package
 (name skapm)
 (synopsis "Application Performance Monitoring (APM) client library for Elastic")
 (depends
  (ocaml (>= 4.08.0))
  (cohttp-lwt-unix (>= 2.5.4))
  (dune (>= 2.5.0))
  (dune-build-info (>= 2.5.0))
  (ppx_deriving (>= 4.5))
  (ppx_deriving_yojson (>= 3.5.3))
  (ptime (>= 0.8.5))
  (mtime (< 2.0.0))
  (uri (>= 3.0.0))
  (uuidm (>= 0.9.7))
  (yojson (>= 1.6.0))))
