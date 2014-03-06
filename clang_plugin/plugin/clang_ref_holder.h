#ifndef CLANG_REF_HOLDER_H
#define CLANG_REF_HOLDER_H

#include "clang_ref.h"
#include "clang_type_traits.h"
#include "heterogenous_container.h"

template<typename T>
struct concrete_ref_holder
  : heterogenous_container<concrete_ref_holder>::concrete<T>
{
  std::vector<typename clang_type<T>::type> values;

  clang_ref<T> create (typename clang_type<T>::type value)
  {
    clang_ref<T> ref = { values.size () };
    values.push_back (value);
    return ref;
  }

  typename clang_type<T>::type retrieve (clang_ref<T> ref)
  {
    return values.at (ref.id);
  }
};


struct clang_ref_holder
  : heterogenous_container<concrete_ref_holder>
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
