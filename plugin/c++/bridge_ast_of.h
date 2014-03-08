#ifndef BRIDGE_AST_OF_H
#define BRIDGE_AST_OF_H

#include "clang_context.h"
#include "clang_type_traits.h"

template<typename T>
ptr<T> bridge_ast_of (typename clang_type<T>::type D,
                      clang_context &ctx);

#endif /* BRIDGE_AST_OF_H */
