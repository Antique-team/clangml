#include "ocaml++.h"

struct value_of_context::data
{
  // Single root containing heterogenous array (record) of the form
  // type t = {
  //   types : ctyp array;
  //   decls : decl array;
  //   exprs : expr array;
  //   ...
  // }
  // The order of these is not necessarily defined, but is currently
  // the order of appearance within the *_bridge.cpp files. It is
  // undefined whether sloc_bridge or ast_bridge are first, but the
  // types defined in those modules can be assumed to be subsequent
  // in this array.
  value cache;
  // Postponed to_value actions
  std::vector<std::function<void ()>> postponed;

  data ();
  ~data ();

  // Print the contents of the cache.
  void dump (char const *msg, ...) const;

  // Get array for typed cache.
  value get   (size_t type) const;
  // Get single value from typed cache.
  value get   (size_t type, size_t index) const;

  // Set a value in the typed cache.
  void set    (size_t type, size_t index, value result) const;

  // Resize the cache to have at least space for max_id.
  void resize (size_t type, size_t max_id) const;
};


inline value_of_context::data const *
value_of_context::operator -> ()
{
  assert (self != nullptr);
  return self;
}
