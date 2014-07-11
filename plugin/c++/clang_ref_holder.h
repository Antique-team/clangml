#ifndef CLANG_REF_HOLDER_H
#define CLANG_REF_HOLDER_H

#include "clang_ref.h"
#include "clang_type_traits.h"
#include "heterogenous_container.h"

template<typename T>
struct concrete_clang_ref_holder
  : heterogenous_container<concrete_clang_ref_holder>::concrete<T>
{
  typedef typename clang_type<T>::type value_type;
  typedef std::vector<value_type> list_type;

  list_type values;

  // O(n), for no good reason. It should probably be O(log n).
  clang_ref<T> create (value_type value)
  {
    typename list_type::const_iterator it = values.begin ();
    typename list_type::const_iterator et = values.end   ();
    while (it != et)
      {
        if (*it == value)
          return { size_t (it - values.begin ()) };
        ++it;
      }

    clang_ref<T> ref = { values.size () + 1 };
    values.push_back (value);
    return ref;
  }

  // O(1), will be O(log n) if the above is.
  value_type retrieve (clang_ref<T> ref)
  {
    return values.at (ref.id - 1);
  }
};


struct clang_ref_holder
  : heterogenous_container<concrete_clang_ref_holder>
{
  template<typename T>
  clang_ref<T> create (typename clang_type<T>::type value)
  {
    return get_concrete<T> ().create (value);
  }

  template<typename T>
  typename clang_type<T>::type retrieve (clang_ref<T> ref)
  {
    return get_concrete<T> ().retrieve (ref);
  }
};

#endif /* CLANG_REF_HOLDER_H */
