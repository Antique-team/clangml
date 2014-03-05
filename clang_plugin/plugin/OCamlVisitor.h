#ifndef OCAML_VISITOR_H
#define OCAML_VISITOR_H

#include "clang_context.h"
#include "clang_type_traits.h"

template<typename T>
ptr<T> bridge_ast_of (typename clang_type<T>::type D,
                      clang_context &ctx);

#endif /* OCAML_VISITOR_H */
