extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <cstdio>
#include <stdexcept>
#include <type_traits>
#include <typeinfo>

#include "OCamlVisitor.h"

using namespace hello_cpp;


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  /****************************************************
   * Stack management
   */

  std::vector<adt_ptr> stack;
  std::vector<size_t> markers;

  template<typename T>
  static ptr<T> adt_cast (adt_ptr adt)
  {
    ptr<T> p = boost::dynamic_pointer_cast<T> (adt);
    if (!p)
      {
        printf ("Expected type %s, but actually type %s\n",
                typeid (T).name (), typeid (*adt).name ());
        throw std::bad_cast ();
      }
    return p;
  }

  struct dynamic
  {
    adt_ptr adt;

    template<typename T>
    operator ptr<T> () const
    {
      return adt_cast<T> (adt);
    }
  };

  struct dynamic_list
  {
    std::vector<adt_ptr> adt_list;

    template<typename T>
    operator std::vector<ptr<T>> () const
    {
      std::vector<ptr<T>> list;
      list.reserve (adt_list.size ());
      std::transform (adt_list.begin (), adt_list.end (), std::back_inserter (list),
                      [] (adt_ptr adt) { return adt_cast<T> (adt); });
      return list;
    }
  };

  dynamic pop ()
  {
    if (stack.empty ())
      throw std::runtime_error ("empty stack");
    assert (!stack.empty ());
    adt_ptr p = stack.back ();
    stack.pop_back ();

    return dynamic { p };
  }

  template<typename Derived>
  void push (ptr<Derived> p)
  {
    static_assert (std::is_base_of<OCamlADTBase, Derived>::value,
                   "can only push OCamlADTBase derived instances");
    stack.push_back (p);
  }


  // Set a stack marker.
  void push_mark ()
  {
    markers.push_back (stack.size ());
  }

  // Get the number of elements pushed since the last marker.
  size_t pop_mark ()
  {
    size_t marker = markers.back ();
    assert (marker <= stack.size ());
    markers.pop_back ();
    return stack.size () - marker;
  }

  // Get a list of everything that was pushed since the last marker.
  dynamic_list pop_shard ()
  {
    // Get last marker.
    size_t marker = pop_mark ();

    // Copy the last size-marker elements to the list.
    std::vector<adt_ptr> list;
    list.reserve (marker);
    list.insert (list.begin (), stack.end () - marker, stack.end ());

    // Pop them off the stack.
    stack.erase (stack.end () - marker, stack.end ());

    return dynamic_list { move (list) };
  }


  /****************************************************
   * Unary/binary operators (helpers)
   */

  void consume_unaryop (UnaryOp op)
  {
    ptr<Expr> arg = pop ();

    push (mkUnaryOperator
          (op, arg));
  }


  void consume_binop (BinaryOp op)
  {
    ptr<Expr> rhs = pop ();
    ptr<Expr> lhs = pop ();

    push (mkBinaryOperator
          (op, lhs, rhs));
  }

public:
  /****************************************************
   * Unary/binary operators
   */

// Code taken from <clang/AST/RecursiveASTVisitor.h>

// All unary operators.
#define UNARYOP_LIST()                          \
  OPERATOR(PostInc)   OPERATOR(PostDec)         \
  OPERATOR(PreInc)    OPERATOR(PreDec)          \
  OPERATOR(AddrOf)    OPERATOR(Deref)           \
  OPERATOR(Plus)      OPERATOR(Minus)           \
  OPERATOR(Not)       OPERATOR(LNot)            \
  OPERATOR(Real)      OPERATOR(Imag)            \
  OPERATOR(Extension)

// All binary operators (excluding compound assign operators).
#define BINOP_LIST() \
  OPERATOR(PtrMemD)              OPERATOR(PtrMemI)    \
  OPERATOR(Mul)   OPERATOR(Div)  OPERATOR(Rem)        \
  OPERATOR(Add)   OPERATOR(Sub)  OPERATOR(Shl)        \
  OPERATOR(Shr)                                       \
                                                      \
  OPERATOR(LT)    OPERATOR(GT)   OPERATOR(LE)         \
  OPERATOR(GE)    OPERATOR(EQ)   OPERATOR(NE)         \
  OPERATOR(And)   OPERATOR(Xor)  OPERATOR(Or)         \
  OPERATOR(LAnd)  OPERATOR(LOr)                       \
                                                      \
  OPERATOR(Assign)                                    \
  OPERATOR(Comma)

// All compound assign operators.
#define CAO_LIST()                                                      \
  OPERATOR(Mul) OPERATOR(Div) OPERATOR(Rem) OPERATOR(Add) OPERATOR(Sub) \
  OPERATOR(Shl) OPERATOR(Shr) OPERATOR(And) OPERATOR(Or)  OPERATOR(Xor)


#define OPERATOR(OP)							\
  bool TraverseUnary##OP (clang::UnaryOperator *op)			\
  {									\
    Base::TraverseUnary##OP (op);					\
    consume_unaryop (UO_##OP);						\
    return true;							\
  }

  UNARYOP_LIST ()
#undef OPERATOR


#define OPERATOR(OP)							\
  bool TraverseBin##OP (clang::BinaryOperator *op)			\
  {									\
    Base::TraverseBin##OP (op);						\
    consume_binop (BO_##OP);						\
    return true;							\
  }

  BINOP_LIST ()
#undef OPERATOR


#define OPERATOR(OP)							\
  bool TraverseBin##OP##Assign (clang::CompoundAssignOperator *op)	\
  {									\
    Base::TraverseBin##OP##Assign (op);					\
    consume_binop (BO_##OP##Assign);					\
    return true;							\
  }

  CAO_LIST ()
#undef OPERATOR

#undef UNARYOP_LIST
#undef BINOP_LIST
#undef CAO_LIST


  /****************************************************
   * Literals
   */

#define INTEGERLITERAL(CLASS, BASE)
  bool TraverseIntegerLiteral (clang::IntegerLiteral *lit)
  {
    Base::TraverseIntegerLiteral (lit);

    push (mkIntegerLiteral
          (lit->getValue ().getSExtValue ()));

    return true;
  }


#define CHARACTERLITERAL(CLASS, BASE)
  bool TraverseCharacterLiteral (clang::CharacterLiteral *lit)
  {
    Base::TraverseCharacterLiteral (lit);

    push (mkCharacterLiteral
          (lit->getValue ()));

    return true;
  }


#define FLOATINGLITERAL(CLASS, BASE)
  bool TraverseFloatingLiteral (clang::FloatingLiteral *lit)
  {
    Base::TraverseFloatingLiteral (lit);

    push (mkFloatingLiteral
          (lit->getValue ().convertToDouble ()));

    return true;
  }


#define STRINGLITERAL(CLASS, BASE)
  bool TraverseStringLiteral (clang::StringLiteral *lit)
  {
    Base::TraverseStringLiteral (lit);

    push (mkStringLiteral
          (lit->getString ()));

    return true;
  }


  /****************************************************
   * Statements
   */

#define RETURNSTMT(CLASS, BASE)
  bool TraverseReturnStmt (clang::ReturnStmt *stmt)
  {
    Base::TraverseReturnStmt (stmt);

    ptr<Expr> expr = pop ();
    push (mkReturnStmt (expr));

    return true;
  }


#define COMPOUNDSTMT(CLASS, BASE)
  bool TraverseCompoundStmt (clang::CompoundStmt *stmt)
  {
    push_mark ();
    Base::TraverseCompoundStmt (stmt);

    std::vector<ptr<Stmt>> stmts = pop_shard ();
    push (mkCompoundStmt (stmts));

    return true;
  }


#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, BASE)					\
  bool Traverse##CLASS (clang::CLASS *stmt)			\
  {								\
    puts (__func__);						\
    Base::Traverse##CLASS (stmt);				\
    return true;						\
  }
#include <clang/AST/StmtNodes.inc>
#undef STMT
#undef ABSTRACT_STMT


  /****************************************************
   * Types
   */

#define ABSTRACT_TYPE(CLASS, BASE)
#define TYPE(CLASS, BASE)					\
  bool Traverse##CLASS##Type (clang::CLASS##Type *type)		\
  {								\
    puts (__func__);						\
    Base::Traverse##CLASS##Type (type);				\
    return true;						\
  }
#include <clang/AST/TypeNodes.def>
#undef TYPE
#undef ABSTRACT_TYPE


  /****************************************************
   * TypeLocs
   */

  bool TraverseTypeLoc (clang::TypeLoc typeLoc)
  {
    push_mark ();
    Base::TraverseTypeLoc (typeLoc);
    size_t marker = pop_mark ();
    if (marker == 0)
      {
        printf ("WARNING: %s creates dummy TypeLoc, as derived function did not produce any\n",
                __func__);
        push (mkBuiltinTypeLoc (BT_Void));
      }
    else if (marker > 1)
      {
        printf ("WARNING: %s drops all but most recent (out of %lu) TypeLoc\n",
                __func__, marker);
        // Keep the last one
        while (--marker) pop ();
      }
    return true;
  }

  static BuiltinType translate_builtin_type (clang::BuiltinType::Kind kind)
  {
    switch (kind)
      {
#define BUILTIN_TYPE(Id, SingletonId)	\
      case clang::BuiltinType::Id:	\
        return BT_##Id;
#include <clang/AST/BuiltinTypes.def>
#undef BUILTIN_TYPE
      }
    throw std::runtime_error ("invalid builtin type");
  }

#if 1
  bool TraverseBuiltinTypeLoc (clang::BuiltinTypeLoc typeLoc)
  {
    Base::TraverseBuiltinTypeLoc (typeLoc);

    BuiltinType bt = translate_builtin_type (typeLoc.getTypePtr ()->getKind ());

    push (mkBuiltinTypeLoc (bt));

    return true;
  }

  bool TraverseConstantArrayTypeLoc (clang::ConstantArrayTypeLoc typeLoc)
  {
    Base::TraverseConstantArrayTypeLoc (typeLoc);

    ptr<TypeLoc> inner = pop ();
    push (mkConstantArrayTypeLoc (inner));

    return true;
  }

  bool TraverseFunctionNoProtoTypeLoc (clang::FunctionNoProtoTypeLoc typeLoc)
  {
    Base::TraverseFunctionNoProtoTypeLoc (typeLoc);

    ptr<TypeLoc> inner = pop ();
    push (mkFunctionNoProtoTypeLoc (inner));

    return true;
  }

  bool TraverseTypedefTypeLoc (clang::TypedefTypeLoc typeLoc)
  {
    Base::TraverseTypedefTypeLoc (typeLoc);

    //ptr<Stmt> stmt = pop ();
    push (mkTypedefTypeLoc ());

    return true;
  }


#else
#define ABSTRACT_TYPELOC(CLASS, BASE)
#define TYPELOC(CLASS, BASE)					\
  bool Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc typeLoc)	\
  {								\
    puts (__func__);						\
    push_mark ();						\
    Base::Traverse##CLASS##TypeLoc (typeLoc);			\
    size_t marker = pop_mark ();				\
    while (marker--) pop ();					\
    push (mkBuiltinTypeLoc (BT_Void));				\
    return true;						\
  }
#include <clang/AST/TypeLocNodes.def>
#undef TYPELOC
#undef ABSTRACT_TYPELOC
#endif


  /****************************************************
   * Declarations
   */

#define FUNCTION(CLASS, BASE)
  bool TraverseFunctionDecl (clang::FunctionDecl *decl)
  {
    Base::TraverseFunctionDecl (decl);

    ptr<Stmt> body = pop ();
    ptr<TypeLoc> type = pop ();
    clang::IdentifierInfo *info = decl->getNameInfo ().getName ().getAsIdentifierInfo ();
    assert (info);
    clang::StringRef name = info->getName ();

    push (mkFunctionDecl (type, name, body));

    return true;
  }


#define TYPEDEF(CLASS, BASE)
  bool TraverseTypedefDecl (clang::TypedefDecl *decl)
  {
    Base::TraverseTypedefDecl (decl);

    ptr<TypeLoc> type = pop ();

    push (mkTypedefDecl (type));

    return true;
  }


#define TRANSLATIONUNIT(CLASS, BASE)
  bool TraverseTranslationUnitDecl (clang::TranslationUnitDecl *decl)
  {
    push_mark ();
    Base::TraverseTranslationUnitDecl (decl);

    std::vector<ptr<Decl>> decls = pop_shard ();
    push (mkTranslationUnitDecl (decls));

    return true;
  }


#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)					\
  bool Traverse##CLASS##Decl (clang::CLASS##Decl *decl)		\
  {								\
    puts (__func__);						\
    Base::Traverse##CLASS##Decl (decl);				\
    return true;						\
  }
#include <clang/AST/DeclNodes.inc>
#undef DECL
#undef ABSTRACT_DECL


  /****************************************************
   * Final result
   */

  ptr<Decl> result ()
  {
    //assert (stack.size () == 1);
    return pop ();
  }
};


ptr<Decl>
adt_of_clangAST (clang::TranslationUnitDecl const *D)
{
  OCamlVisitor visitor;
  visitor.TraverseDecl (const_cast<clang::TranslationUnitDecl *> (D));
  return visitor.result ();
}
