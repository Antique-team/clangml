#!/bin/bash



INCLUDE_PART=-I,/usr/local/lib/ocaml/camlp4,-I,/usr/local/lib/ocaml/camlp4/Camlp4Parsers

LINK_PART=/usr/local/lib/ocaml/dynlink.cma,\
/usr/local/lib/ocaml/camlp4/camlp4fulllib.cma


ocamlbuild main.byte -cflags ${INCLUDE_PART} -lflags ${LINK_PART}
