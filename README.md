# Tim - command line time tracker written in OCaml

## Dependencies

opam install yojson ANSITerminal

## Building

corebuild -pkgs yojson,ANSITerminal -I src/ tim.native
