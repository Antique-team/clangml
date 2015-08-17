#include "ocaml++/private.h"

#include <sstream>
#include <stdexcept>


#define DEBUG_DUMP		0
#define DEBUG_INIT		0
#define DEBUG_POSTPONE		0
#define DEBUG_RESIZE		0


value_of_context::data::data ()
  : cache (caml_alloc (OCamlADTBase::num_type_ids, 0))
{
  caml_register_generational_global_root (&cache);
}


value_of_context::data::~data ()
{
  caml_remove_generational_global_root (&cache);
}


void
value_of_context::data::dump (char const *msg, ...) const
{
  assert (cache != 0);

  printf ("cache ");
  va_list ap;
  va_start (ap, msg);
  vprintf (msg, ap);
  va_end (ap);
  printf (":\n");

  size_t type_count = caml_array_length (cache);
  for (size_t type = 0; type < type_count; type++)
    {
      printf ("%2zu: ", type);

      value typed_cache = Field (cache, type);
      if (typed_cache == Val_unit)
        {
          printf ("(0) []\n");
        }
      else
        {
          size_t length = caml_array_length (typed_cache);
          printf ("(%zu) [", length);
          for (size_t i = 0; i < length; i++)
            {
              value val = Field (typed_cache, i);
              printf ("%lu", val);
              if (i < length - 1)
                printf (";");
            }
          printf ("]\n");
        }
    }
}


value
value_of_context::data::get (size_t type) const
{
  // get cache for type id
  assert (type < caml_array_length (cache));
  return (Field (cache, type));
}


void
value_of_context::data::set (size_t type, size_t index, value result) const
{
  resize (type, index);

  // get cache for type id
  value typed_cache = get (type);
  assert (typed_cache != Val_unit);

  // ensure enough space in the cache for that type
  assert (index < caml_array_length (typed_cache));

  // we only store heap values
  assert (Is_block (result));
  assert (result != Val_unit);

  // save value in typed cache
  Store_field (typed_cache, index, result);
}


value
value_of_context::data::get (size_t type, size_t index) const
{
#if DEBUG_DUMP
  dump ("on get()");
#endif

  // get cache for type id
  value typed_cache = get (type);

  // no values for this type
  if (typed_cache == Val_unit)
    return Val_unit;

  // this index doesn't exist (yet)
  if (index >= caml_array_length (typed_cache))
    return Val_unit;

  return Field (typed_cache, index);
}


void
value_of_context::data::resize (size_t type, size_t max_id) const
{
  CAMLparam0 ();
  CAMLlocal3 (old_cache, new_cache, tmp);

#if DEBUG_DUMP
  dump ("before resize");
#endif

  // get cache for type id
  assert (type < caml_array_length (cache));
  old_cache = Field (cache, type);

  if (old_cache == Val_unit)
    {
      new_cache = caml_alloc (max_id + max_id / 8 + 10, 0);
#if DEBUG_INIT
      printf ("created new ocaml value cache for type id %2zu of size %ld (at least %ld)\n",
              type,
              caml_array_length (new_cache),
              max_id);
#endif
    }
  else
    {
      if (max_id < caml_array_length (old_cache))
        CAMLreturn0;

      new_cache = caml_alloc (max_id + max_id / 8 + 10, 0);

      size_t length = caml_array_length (old_cache);
      for (size_t i = 0; i < length; i++)
        {
          tmp = Field (old_cache, i);
#if DEBUG_RESIZE
          printf ("%zu/%zu: %ld\n", i, length, tmp);
#endif
          if (Is_block (tmp))
            {
              assert (i < caml_array_length (new_cache));
              Store_field (new_cache, i, tmp);
            }
        }

#if DEBUG_RESIZE
      printf ("resized ocaml value cache for type %2zu from %ld to %ld (at least %ld)\n",
              type,
              caml_array_length (old_cache),
              caml_array_length (new_cache),
              max_id);
#endif
    }

  assert (new_cache != Val_unit);
  Store_field (cache, type, new_cache);

#if DEBUG_DUMP
  dump ("after resize");
#endif

  CAMLreturn0;
}


value_of_context::value_of_context ()
  : self (nullptr)
{
}


value_of_context::~value_of_context ()
{
  delete self;
}


void
value_of_context::resize (size_t type, size_t max_id)
{
  if (self == nullptr)
    {
      self = new data;
      for (size_t type = 0; type < OCamlADTBase::num_type_ids; type++)
        {
          size_t local_ids = OCamlADTBase::num_local_ids.at (type);
          if (local_ids > 0)
            self->resize (type, local_ids);
        }
    }
  self->resize (type, max_id);
}


value
value_of_context::get (size_t type) const
{
  CAMLparam0 ();
  CAMLlocal2 (typed_cache, slice);

  typed_cache = self->get (type);

  // Count the number of actual values.
  size_t size = 0;
  while (Field (typed_cache, size) != Val_unit)
    size++;

  slice = caml_alloc (size, 0);
  while (size--)
    Store_field (slice, size, Field (typed_cache, size));

  CAMLreturn (slice);
}


void
value_of_context::postpone (std::function<void ()> fun)
{
#if DEBUG_POSTPONE
  printf ("postponed actions: %zu\n", self->postponed.size ());
#endif
  self->postponed.push_back (fun);
}


void
value_of_context::finish ()
{
#if DEBUG_POSTPONE
  printf ("performing %zu postponed actions\n", self->postponed.size ());
  fflush (stdout);
#endif
  while (!self->postponed.empty ())
    {
      self->postponed.back () ();
      self->postponed.pop_back ();
    }
}
