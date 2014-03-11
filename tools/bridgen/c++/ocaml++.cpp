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
    : cache (caml_alloc (max_id + max_id / 512 + 1, 0))
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

  void resize (size_t max_id)
  {
    if (max_id < caml_array_length (cache))
      return;

    CAMLparam0 ();
    CAMLlocal1 (new_cache);

    new_cache = caml_alloc (max_id + max_id / 512 + 1, 0);

    size_t length = caml_array_length (cache);
    for (size_t i = 0; i < length; i++)
      {
        assert (Is_block (Field (cache, i)));
        Store_field (new_cache, i, Field (cache, i));
      }

#if 0
    printf ("resized ocaml value cache from %ld to %ld\n",
            caml_array_length (cache),
            caml_array_length (new_cache));
#endif

    caml_modify_generational_global_root (&cache, new_cache);

    CAMLreturn0;
  }
};


value_of_context::value_of_context ()
  : self (nullptr)
{
}

value_of_context::~value_of_context ()
{
  delete self;
}

void
value_of_context::resize (size_t max_id)
{
  if (!self)
    self = new data (max_id);
  else
    self->resize (max_id);
}


inline value_of_context::data *
value_of_context::operator -> ()
{
  assert (self != nullptr);
  return self;
}


value
OCamlADTBase::to_value (value_of_context &ctx)
{
  CAMLparam0 ();
  CAMLlocal1 (result);

  result = ctx->get (id);

  if (!Int_val (result))
    {
      result = ToValue (ctx);
      values_created++;

      ctx->set (id, result);
    }
  else
    {
      assert (Is_block (result));
    }

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
