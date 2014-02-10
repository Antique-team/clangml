#ifndef DYNAMIC_STACK_H
#define DYNAMIC_STACK_H

#include <stdexcept>
#include <type_traits>
#include <typeinfo>

#include <cxxabi.h>

namespace dynamic_stack_detail
{

  static std::string
  demangle (std::type_info const &ti)
  {
    size_t length;
    int status;
    if (char *name = __cxxabiv1::__cxa_demangle (ti.name (), NULL, &length, &status))
      {
        std::string result (name, length);
        free (name);
        return result;
      }
#if 0
    switch (status)
      {
      case -1: return "A memory allocation failure occurred.";
      case -2: return "Type name is not a valid name under the C++ ABI mangling rules.";
      case -3: return "One of the arguments is invalid.";
      }
#endif
    return ti.name ();
  }

  template<typename T>
  static std::string
  name (T const &p)
  {
    return demangle (typeid (p));
  }

  template<typename T>
  static std::string
  name ()
  {
    return demangle (typeid (T));
  }


  template<typename T>
  static ptr<T>
  adt_cast (adt_ptr adt)
  {
    ptr<T> p = boost::dynamic_pointer_cast<T> (adt);
    if (!p)
      {
        printf ("Expected type %s, but actually type %s\n",
                name<T> ().c_str (), name (*adt).c_str ());
        throw std::bad_cast ();
      }
    return p;
  }


  using namespace hello_cpp;

  // Special case for Expr -> Stmt
  template<>
  ptr<Stmt>
  adt_cast (adt_ptr adt)
  {
    ptr<Stmt> p = boost::dynamic_pointer_cast<Stmt> (adt);
    if (!p)
      return mkExprStmt (adt_cast<Expr> (adt));
    return p;
  }

}


/****************************************************
 * Stack management
 */

class dynamic_stack
{
  adt_list stack;
  std::vector<size_t> markers;

public:
  struct element
  {
    adt_ptr px;

    template<typename T>
    operator ptr<T> () const
    {
      assert (px);
      return dynamic_stack_detail::adt_cast<T> (px);
    }

    template<typename T>
    operator option<T> () const
    {
      if (!px)
        return nullptr;
      return dynamic_stack_detail::adt_cast<T> (px);
    }

    explicit operator bool () const { return !!px; }
  };

  struct range
  {
    adt_list data;

    template<typename T>
    operator list<T> () const
    {
      list<T> l;
      l.reserve (data.size ());
      std::transform (data.begin (), data.end (),
                      std::back_inserter (l),
                      dynamic_stack_detail::adt_cast<T>);
      return l;
    }
  };

  element pop ()
  {
    if (stack.empty ())
      throw std::runtime_error ("empty stack");
    assert (!stack.empty ());
    adt_ptr p = stack.back ();
    stack.pop_back ();

    return element { p };
  }

  template<template<typename> class Ptr, typename Derived>
  void push (Ptr<Derived> p)
  {
    // Requiring this specific template already makes pushing option
    // impossible, because it's option<typename, bool>, but just in case
    // that changes in the future, we explicitly check for it here.
    static_assert (!std::is_base_of<option<Derived>, Ptr<Derived>>::value,
                   "cannot push option types");
    // The push_back below would fail if this is not the case, but this
    // gives a more descriptive error message.
    static_assert (std::is_base_of<OCamlADTBase, Derived>::value,
                   "can only push OCaml bridge types");
    // While members of a sum type may contain options, they can never
    // be null, themselves.
    assert (p);
    stack.push_back (p);
  }


  // Set a stack marker.
  void push_mark ()
  {
    markers.push_back (stack.size ());
  }

  // Get the number of elements pushed since the last marker.
  size_t pop_mark ()
  {
    size_t marker = markers.back ();
    assert (marker <= stack.size ());
    markers.pop_back ();
    return stack.size () - marker;
  }

  // Get a list of everything that was pushed since the last marker
  // in order of pushing (reverse of popping).
  range pop_marked ()
  {
    // Get last marker.
    size_t marker = pop_mark ();

    // Copy the last size-marker elements to the list.
    adt_list l;
    l.reserve (marker);
    l.insert (l.begin (), stack.end () - marker, stack.end ());

    // Pop them off the stack.
    stack.erase (stack.end () - marker, stack.end ());

    return range { move (l) };
  }

  size_t size () const { return stack.size (); }
  bool empty () const { return stack.empty (); }
};


#endif /* DYNAMIC_STACK_H */
