#include "ocaml++.h"


size_t OCamlADTBase::values_created;
size_t OCamlADTBase::ids_assigned;
size_t OCamlADTBase::bytes_allocated;


void
OCamlADTBase::reset_statistics ()
{
  OCamlADTBase::ids_assigned    = 0;
  OCamlADTBase::values_created  = 0;
  OCamlADTBase::bytes_allocated = 0;
}


void
OCamlADTBase::print_statistics ()
{
#define DISPLAY_ACCOUNTING 0
#if DISPLAY_ACCOUNTING
  printf ("%lu ids assigned\n", OCamlADTBase::ids_assigned);
  printf ("%lu values created\n", OCamlADTBase::values_created);

  if (OCamlADTBase::bytes_allocated > 1024 * 1024)
    printf ("%.2fMiB allocated (C++)\n", float (OCamlADTBase::bytes_allocated) / 1024 / 1024);
  else if (OCamlADTBase::bytes_allocated > 1024)
    printf ("%.2fKiB allocated (C++)\n", float (OCamlADTBase::bytes_allocated) / 1024);
  else
    printf ("%luB allocated (C++)\n", OCamlADTBase::bytes_allocated);

  printf ("%luB per object (avg, C++)\n", OCamlADTBase::bytes_allocated / OCamlADTBase::ids_assigned);
#endif
}


struct value_of_context::data
{
  value cache;

  data (size_t max_id)
    : cache (caml_alloc (max_id + 1, 0))
  {
    caml_register_generational_global_root (&cache);
  }

  ~data ()
  {
    caml_remove_generational_global_root (&cache);
  }

  void set (size_t index, value result)
  {
    assert (index < caml_array_length (cache));
    Store_field (cache, index, result);
  }

  value get (size_t index)
  {
    assert (index < caml_array_length (cache));
    return Field (cache, index);
  }
};


value_of_context::value_of_context (size_t max_id)
  : self (new data (max_id))
{
}

value_of_context::~value_of_context ()
{
  delete self;
}


value
OCamlADTBase::to_value (value_of_context &ctx)
{
  CAMLparam0 ();
  CAMLlocal1 (result);

#define VALUE_SHARING 1
#if VALUE_SHARING
  result = ctx->get (id);

  if (!result)
    {
#endif
      result = ToValue (ctx);
      values_created++;

#if VALUE_SHARING
      ctx->set (id, result);
    }
#endif

  CAMLreturn (result);
}


#define STATIC_MEMORY 0
#if STATIC_MEMORY
void *
OCamlADTBase::operator new (size_t size)
{
  static char mem[20 * 1024 * 1024];
  static char *cur = mem;
  void *ptr = cur;
  cur += size;
  bytes_allocated += size;
  return ptr;
}

void
OCamlADTBase::operator delete (void *ptr)
{
}
#else
void *
OCamlADTBase::operator new (size_t size)
{
  bytes_allocated += size;
  return ::operator new (size);
}

void
OCamlADTBase::operator delete (void *ptr)
{
  return ::operator delete (ptr);
}
#endif
