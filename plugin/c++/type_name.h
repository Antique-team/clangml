#ifndef TYPE_NAME_H
#define TYPE_NAME_H

#include <string>
#include <typeinfo>

#include "ocaml++.h"

std::string demangle (std::type_info const &ti);


template<typename T>
static inline std::string type_name (ptr<T> p)
{ return "ptr<" + demangle (typeid (*p)) + ">"; }

template<typename T>
static inline std::string type_name (T *p)
{ return demangle (typeid (*p)) + "*"; }

template<typename T>
static inline std::string type_name (T p)
{ return demangle (typeid (p)); }


#endif /* TYPE_NAME_H */
