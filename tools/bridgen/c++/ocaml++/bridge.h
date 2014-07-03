#ifndef OCAMLPP_BRIDGE_H
#define OCAMLPP_BRIDGE_H

#include "ocaml++.h"

// Define these here, so it's only defined where the bridge types are created.

template<typename Derived>
size_t const OCamlADT<Derived>::class_type_id =
  (num_local_ids.resize (num_type_ids + 1),
   num_type_ids++);

template<typename Derived>
bool const OCamlADT<Derived>::class_initialised = class_init (class_type_id, typeid (Derived));

#endif // OCAMLPP_BRIDGE_H
