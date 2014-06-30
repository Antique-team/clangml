#include "dynamic_stack.h"
#include "trace.h"

#include <sstream>

#define TRACING 0

namespace dynamic_stack_detail
{
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

#if TRACING
  printf ("%*s  pop () -> %s@%zu\n", tracer::level - 2, "",
          demangle (typeid (*p)).c_str (), p->id);
#endif
  return element { p };
}

adt_ptr
dynamic_stack::top () const
{
  return stack.back ();
}


void
dynamic_stack::push_ptr (adt_ptr p)
{
#if TRACING
  printf ("%*s  push (%s@%zu)\n", tracer::level - 2, "",
          demangle (typeid (*p)).c_str (), p->id);
#endif
  stack.push_back (p);
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


void
dynamic_stack::dump (std::ostringstream &os, size_t count)
{
  for (size_t i = 0; i < count; i++)
    {
      adt_ptr p = *(stack.end () - (i + 1));
      os << "\n\t  [" << i << "] "
         << demangle (typeid (*p))
         << "@" << p->id ();
    }
}
