#ifndef CLANG_REF_HOLDER_H
#define CLANG_REF_HOLDER_H

#include "clang_ref.h"
#include "clang_type_traits.h"

#include <memory>
#include <vector>


struct clang_ref_holder
{
  static size_t type_count;

  struct abstract
  {
    virtual ~abstract () { }
  };

  template<typename T>
  struct concrete
    : abstract
  {
    static size_t const id;

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

  clang_ref_holder ()
    : types (type_count)
  {
  }

  template<typename T>
  clang_ref<T> create (typename clang_type<T>::type value)
  {
    std::shared_ptr<concrete<T>> con;
    std::shared_ptr<abstract> abs = types.at (concrete<T>::id);
    if (!abs)
      {
        con.reset (new concrete<T>);
        types[con->id] = con;
      }
    else
      {
        con = std::dynamic_pointer_cast<concrete<T>> (abs);
        if (!con)
          throw std::runtime_error ("concrete value holder does not correspond to "
                                    "expected concrete type");
      }
    return con->create (value);
  }

  template<typename T>
  typename clang_type<T>::type retrieve (clang_ref<T> ref)
  {
    std::shared_ptr<abstract> abs = types.at (concrete<T>::id);
    if (!abs)
      throw std::runtime_error ("value holder contains no values for requested type");

    std::shared_ptr<concrete<T>> con = std::dynamic_pointer_cast<concrete<T>> (abs);
    if (!con)
      throw std::runtime_error ("concrete value holder does not correspond to "
                                "expected concrete type");

    return con->retrieve (ref);
  }

private:
  std::vector<std::shared_ptr<abstract>> types;
};


template<typename T>
size_t const clang_ref_holder::concrete<T>::id = clang_ref_holder::type_count++;

#endif /* CLANG_REF_HOLDER_H */
