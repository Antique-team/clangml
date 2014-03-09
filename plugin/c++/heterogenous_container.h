#ifndef HETEROGENOUS_CONTAINER_H
#define HETEROGENOUS_CONTAINER_H

#include <memory>
#include <vector>


template<template<typename> class Concrete>
struct heterogenous_container
{
private:
  struct abstract
  {
    virtual ~abstract () { }
  };

protected:
  template<typename T>
  Concrete<T> &get_concrete ()
  {
    std::shared_ptr<Concrete<T>> con;
    std::shared_ptr<abstract> abs = types.at (Concrete<T>::id);
    if (!abs)
      {
        con.reset (new Concrete<T>);
        types[con->id] = con;
      }
    else
      {
        con = std::dynamic_pointer_cast<Concrete<T>> (abs);
        if (!con)
          throw std::runtime_error ("concrete value holder does not correspond to "
                                    "expected concrete type");
      }
    return *con;
  }

  heterogenous_container ()
    : types (type_count)
  {
  }

public:
  template<typename T>
  struct concrete
    : abstract
  {
  private:
    static size_t const id;

    friend struct heterogenous_container<Concrete>;
  };

private:
  static size_t type_count;

  std::vector<std::shared_ptr<abstract>> types;
};


template<template<typename> class Concrete>
size_t heterogenous_container<Concrete>::type_count;

template<template<typename> class Concrete>
template<typename T>
size_t const heterogenous_container<Concrete>::concrete<T>::id = heterogenous_container<Concrete>::type_count++;


#endif /* HETEROGENOUS_CONTAINER_H */
