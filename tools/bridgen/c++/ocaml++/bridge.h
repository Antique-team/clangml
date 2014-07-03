#ifndef OCAMLPP_BRIDGE_H
#define OCAMLPP_BRIDGE_H

#include "ocaml++.h"

// Define these here, so it's only defined where the bridge types are created.

template<typename Derived>
size_t const OCamlADT<Derived>::class_type_id = class_init (typeid (Derived));

#endif // OCAMLPP_BRIDGE_H
