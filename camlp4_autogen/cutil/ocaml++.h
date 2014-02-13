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

#define UNIQUE_IDS 1

/********************************************************
 * Value construction context.
 */

struct value_of_context
{
  struct data;
  data *self;

  value_of_context (size_t max_id);
  ~value_of_context ();

  data *operator -> () { return  self; }
  data &operator *  () { return *self; }

  value_of_context (value_of_context const &other) = delete;
  value_of_context &operator = (value_of_context const &other) = delete;
};



// Base class for all ADT types.
struct OCamlADTBase
{
private:
  virtual value ToValue (value_of_context &ctx) const = 0;

public:
  virtual mlsize_t size () const = 0;
  virtual tag_t tag () const = 0;

  virtual ~OCamlADTBase () { }

  int refcnt = 0;

#if UNIQUE_IDS
  static size_t ids_assigned;
  size_t id = ids_assigned++;
  size_t hash () const { return id; }
#else
  size_t hash () const { return reinterpret_cast<size_t> (this); }
#endif

  static size_t values_created;
  value to_value (value_of_context &ctx);

#if STATIC_MEMORY
  void *operator new (size_t size)
  {
    static char mem[20 * 1024 * 1024];
    static char *cur = mem;
    void *ptr = cur;
    cur += size;
    return ptr;
  }

  void operator delete (void *ptr)
  {
  }
#endif
};


// OCaml bridge object.
template<typename T>
using ptr = boost::intrusive_ptr<T>;

typedef ptr<OCamlADTBase> adt_ptr;


// List of OCaml bridge objects.
template<typename T>
using list = std::vector<ptr<T>>;

typedef list<OCamlADTBase> adt_list;


// OCaml option type.
template<typename T, bool is_adt = std::is_base_of<OCamlADTBase, T>::value>
struct option;

// Specialisation for OCaml bridge object options.
template<typename T>
struct option<T, true>
  : private ptr<T> // don't allow conversion from option to ptr
{
  using ptr<T>::ptr;
  using ptr<T>::operator bool;
  using ptr<T>::operator ->;

  value to_value (value_of_context &ctx) const { return (*this)->to_value (ctx); }
};

// Implementation for all other option types.
template<typename T>
struct option<T, false>
{
private:
  T ob;
  bool some;

public:
  option ()
    : some (false)
  { }

  option (T value)
    : ob (value)
    , some (true)
  { }

  option &operator = (T value)
  {
    ob = value;
    some = true;
    return *this;
  }

  explicit operator bool () const { return some; }

  value to_value (value_of_context &ctx) const;
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

// Recursive entry-point for OCamlADTBase::to_value.
template<typename OCamlADT, typename... Args>
value value_of_adt (value_of_context &ctx, adt_ptr self, Args const &...v);


// Create an OCaml list from a C++ iterator range.
template<typename Iterator>
value value_of_range (value_of_context &ctx, Iterator begin, Iterator end);


/********************************************************
 * value_of
 */

template<typename T>
static inline value
value_of (value_of_context &ctx, std::vector<T> const &v)
{
  return value_of_range (ctx, v.begin (), v.end ());
}


// Non-null pointer.
template<typename T>
typename std::enable_if<std::is_base_of<OCamlADTBase, T>::value, value>::type // TODO: enable_if_t when C++14 arrives
value_of (value_of_context &ctx, ptr<T> ob)
{
  assert (ob);
  return ob->to_value (ctx);
}

// Nullable pointer.
template<typename T>
value
value_of (value_of_context &ctx, option<T> ob)
{
  CAMLparam0 ();
  CAMLlocal1 (option);

  option = Val_int (0);
  if (ob)
    {
      option = caml_alloc (1, 0);
      Store_field (option, 0, ob.to_value (ctx));
    }

  CAMLreturn (option);
}


static inline value value_of (value_of_context &ctx, int v) { return Val_int (v); }
static inline value value_of (value_of_context &ctx, double v) { return caml_copy_double (v); }
static inline value value_of (value_of_context &ctx, int64 v) { return caml_copy_int64 (v); }

static inline value
value_of (value_of_context &ctx, llvm::StringRef v)
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
value_of_range (value_of_context &ctx, Iterator begin, Iterator end)
{
  CAMLparam0 ();
  CAMLlocal3 (start_value, cons_value, tmp_value);

  // It would be much easier to build the list backwards,
  // but we may not have that kind of iterator

  if (begin == end)
    return Val_emptylist;

  start_value = caml_alloc (2, 0);
  // head is first item
  Store_field (start_value, 0, value_of (ctx, *begin));

  cons_value = start_value;

  ++begin;
  for (; begin != end; begin++)
    {
      tmp_value = caml_alloc (2, 0); 
      // tail is not yet fully constructed rest of list
      Store_field (cons_value, 1, tmp_value);

      cons_value = tmp_value;
      Store_field (cons_value, 0, value_of (ctx, *begin));
    }

  // tail of last cons is empty list
  Store_field (cons_value, 1, Val_emptylist);

  CAMLreturn (start_value);
}



/********************************************************
 * value_of_adt implementation.
 */

static inline void
store_fields (value_of_context &ctx, value result, int field)
{
  // recursion end
}

template<typename Arg0, typename... Args>
void store_fields (value_of_context &ctx, value &result, int field, Arg0 const &arg0, Args const &...args)
{
  Store_field (result, field, value_of (ctx, arg0));
  store_fields (ctx, result, field + 1, args...);
}


template<typename OCamlADT, typename... Args>
value
value_of_adt (value_of_context &ctx, OCamlADT const *self, Args const &...v)
{
  assert (sizeof... (v) == self->size ());
  CAMLparam0 ();
  CAMLlocal1 (result);

  result = caml_alloc (self->size (), self->tag ());

  store_fields (ctx, result, 0, v...);

  CAMLreturn (result);
}

template<typename OCamlADT>
value
value_of_adt (value_of_context &ctx, OCamlADT const *self)
{
  return Val_int (self->tag ());
}


/********************************************************
 * option<non-adt>::to_value implementation.
 * Comes last, so all value_ofs are in scope.
 */

template<typename T>
value
option<T, false>::to_value (value_of_context &ctx) const
{
  return value_of (ctx, ob);
}

#endif /* OCAMLPP_H */
