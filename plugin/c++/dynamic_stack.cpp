#include "dynamic_stack.h"

#include <cxxabi.h>

namespace dynamic_stack_detail
{
  std::string
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


  // Special case for Expr -> Stmt
  template<>
  ptr<ast_bridge::Stmt>
  adt_cast (adt_ptr adt)
  {
    using namespace ast_bridge;

    ptr<Stmt> p = boost::dynamic_pointer_cast<Stmt> (adt);
    if (!p)
      {
        ptr<Expr> expr = adt_cast<Expr> (adt);
        if (!expr)
          {
            printf ("Expected either Stmt or Expr, but got type %s\n",
                    name (*adt).c_str ());
            throw std::bad_cast ();
          }
        ptr<Stmt> stmt = mkStmt ();
        stmt->s = mkExprStmt (expr);
        stmt->s_sloc = expr->e_sloc;
        return stmt;
      }
    return p;
  }
}


/****************************************************
 * Stack management
 */

dynamic_stack::element
dynamic_stack::pop ()
{
  if (stack.empty ())
    throw std::runtime_error ("empty stack");
  assert (!stack.empty ());
  adt_ptr p = stack.back ();
  stack.pop_back ();

  return element { p };
}

adt_ptr
dynamic_stack::top () const
{
  return stack.back ();
}


void
dynamic_stack::push_mark ()
{
  markers.push_back (stack.size ());
}


size_t
dynamic_stack::pop_mark ()
{
  size_t marker = markers.back ();
  assert (marker <= stack.size ());
  markers.pop_back ();
  return stack.size () - marker;
}


dynamic_stack::range
dynamic_stack::pop_marked ()
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
