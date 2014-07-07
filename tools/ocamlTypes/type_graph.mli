(*
  Given an association list (string * ocaml_type), return
  the names of the types that are visitable.

  A type is visitable, if and only if it can recursively contain
  a value of itself.

  E.g.
    type t1 =
      | Foo
      | Bar of t2

    type t2 = {
      members : t1 list;
      variant : t3;
    }

    type t3 =
      | Var1
      | Var2

  In the above example, t1 and t2 are visitable, but t3 is not.
  A type T is reachable from another type U, if U is a record type
  containing a member of type T, T list, or T option, or if U is a
  sum type containing at least one variant with a T, T list, or T option.

  The function must_visit will return ["t1"; "t2"].

  There is no requirement on the order of this list. In this example, it
  was lexicographically ordered, but it may be random.
 *)
val must_visit : Sig.ocaml_types -> string list
