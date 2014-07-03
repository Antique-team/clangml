#include "ocaml++/private.h"


#define DEBUG_CREATE		0
#define DEBUG_ACCOUNTING	0

#define STATIC_MEMORY		0


size_t OCamlADTBase::values_created;
size_t OCamlADTBase::num_global_ids;
size_t OCamlADTBase::bytes_allocated;
size_t OCamlADTBase::num_type_ids;
std::vector<size_t> OCamlADTBase::num_local_ids;


size_t
OCamlADTBase::class_init (std::type_info const &ti)
{
  size_t type_id = num_type_ids++;
  printf ("create new ocaml adt with id %2zu: %s\n",
          type_id,
          ti.name ());
  num_local_ids.resize (num_type_ids);
  return type_id;
}


void
OCamlADTBase::reset_statistics ()
{
  OCamlADTBase::num_global_ids  = 0;
  OCamlADTBase::values_created  = 0;
  OCamlADTBase::bytes_allocated = 0;
}


void
OCamlADTBase::print_statistics ()
{
#if DEBUG_ACCOUNTING
  printf ("%lu global ids\n"    , OCamlADTBase::num_global_ids);
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


OCamlADTBase::OCamlADTBase ()
{
  num_global_ids++;
}


size_t
OCamlADTBase::id () const
{
  return local_id ();
}


value
OCamlADTBase::to_value (value_of_context &ctx) const
{
  CAMLparam0 ();
  CAMLlocal1 (result);

  size_t ty = type_id ();
  size_t id = this->id ();

  result = ctx->get (ty, id);

  if (!Int_val (result))
    {
#if DEBUG_CREATE
      printf ("%02zu/%02zu(%03zu): %s\n",
              ty,
              local_id (),
              global_id,
              typeid (*this).name ());
#endif
      result = ToValue (ctx);
      values_created++;
      if (Is_block (result))
        ctx->set (ty, id, result);
    }
  else
    {
      assert (Is_block (result));
    }

  fflush (stdout);

  CAMLreturn (result);
}


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
