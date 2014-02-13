#include "ocaml++.h"

#define VALUE_SHARING 1


size_t OCamlADTBase::ids_assigned;
size_t OCamlADTBase::values_created;


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
