#ifndef OCAMLPP_H
#define OCAMLPP_H

#include <cassert>
#include <vector>

#include <boost/intrusive_ptr.hpp>
#include <type_traits>

extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <llvm/ADT/StringRef.h>


// Base class for all ADT types.
struct OCamlADTBase
{
  virtual mlsize_t size () const = 0;
  virtual tag_t tag () const = 0;
  virtual value ToValue () const = 0;

  virtual ~OCamlADTBase () { }

  int refcnt = 0;
};


template<typename T>
using ptr = boost::intrusive_ptr<T>;

typedef ptr<OCamlADTBase> adt_ptr;


template<typename T, bool is_adt = std::is_base_of<OCamlADTBase, T>::value>
struct option;

template<typename T>
struct option<T, true>
  : private ptr<T> // don't allow conversion from option to ptr
{
  using ptr<T>::ptr;
  using ptr<T>::operator bool;
  using ptr<T>::operator ->;
};

typedef option<OCamlADTBase> adt_option;


static inline void
intrusive_ptr_add_ref (OCamlADTBase *p)
{
  ++p->refcnt;
}

static inline void
intrusive_ptr_release (OCamlADTBase *p)
{
  if (--p->refcnt == 0)
    delete p;
}


/********************************************************
 * Forward declarations.
 */

// Recursive entry-point for OCamlADTBase::ToValue.
template<typename OCamlADT, typename... Args>
value value_of_adt (adt_ptr self, Args const &...v);


// Create an OCaml list from a C++ iterator range.
template<typename Iterator>
value value_of_range (Iterator begin, Iterator end);


/********************************************************
 * value_of
 */

template<typename T>
static inline value
value_of (std::vector<T> const &v)
{
  return value_of_range (v.begin (), v.end ());
}


// Non-null pointer.
template<typename T>
typename std::enable_if<std::is_base_of<OCamlADTBase, T>::value, value>::type // TODO: enable_if_t when C++14 arrives
value_of (ptr<T> ob)
{
  assert (ob);
  return ob->ToValue ();
}

// Nullable pointer.
template<typename T>
typename std::enable_if<std::is_base_of<OCamlADTBase, T>::value, value>::type
value_of (option<T> ob)
{
  CAMLparam0 ();
  CAMLlocal1 (option);

  option = Val_int (0);
  if (ob)
    {
      option = caml_alloc (1, 0);
      Store_field (option, 0, ob->ToValue ());
    }

  CAMLreturn (option);
}


static inline value value_of (int v) { return Val_int (v); }
static inline value value_of (double v) { return caml_copy_double (v); }
static inline value value_of (int64 v) { return caml_copy_int64 (v); }

static inline value
value_of (llvm::StringRef v)
{
  CAMLparam0 ();
  CAMLlocal1 (string);

  string = caml_alloc_string (v.size ());
  memcpy (String_val (string), v.data (), v.size ());

  CAMLreturn (string);
}


/********************************************************
 * value_of_range implementation.
 */

template<typename Iterator>
value
value_of_range (Iterator begin, Iterator end)
{
  CAMLparam0 ();
  CAMLlocal3 (start_value, cons_value, tmp_value);

  // It would be much easier to build the list backwards,
  // but we may not have that kind of iterator

  if (begin == end)
    return Val_emptylist;

  start_value = caml_alloc (2, 0);
  // head is first item
  Store_field (start_value, 0, value_of (*begin));

  cons_value = start_value;

  ++begin;
  for (; begin != end; begin++)
    {
      tmp_value = caml_alloc (2, 0); 
      // tail is not yet fully constructed rest of list
      Store_field (cons_value, 1, tmp_value);

      cons_value = tmp_value;
      Store_field (cons_value, 0, value_of (*begin));
    }

  // tail of last cons is empty list
  Store_field (cons_value, 1, Val_emptylist);

  CAMLreturn (start_value);
}



/********************************************************
 * value_of_adt implementation.
 */

static inline void
store_fields (value result, int field)
{
  // recursion end
}

template<typename Arg0, typename... Args>
void store_fields (value &result, int field, Arg0 const &arg0, Args const &...args)
{
  Store_field (result, field, value_of (arg0));
  store_fields (result, field + 1, args...);
}


template<typename OCamlADT, typename... Args>
value
value_of_adt (OCamlADT const *self, Args const &...v)
{
  assert (sizeof... (v) == self->size ());
  CAMLparam0 ();
  CAMLlocal1 (result);

  result = caml_alloc (self->size (), self->tag ());

  store_fields (result, 0, v...);

  CAMLreturn (result);
}

template<typename OCamlADT>
value
value_of_adt (OCamlADT const *self)
{
  return Val_int (self->tag ());
}


#endif /* OCAMLPP_H */
