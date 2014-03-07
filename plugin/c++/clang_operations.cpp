#include "clang_context.h"
#include "OCamlVisitor.h"

O_BEGIN_DECLS


using namespace bridge_ast;


CAMLprim value
clang_canonical_type (value context, value id)
{
  clang_context &ctx = *reinterpret_cast<clang_context *> (Bp_val (context));
  clang_ref<Ctyp> ref (Unsigned_long_val (id));

  clang::QualType type = ctx.refs.retrieve (ref).getCanonicalType ();

  ptr<Ctyp> ctyp = bridge_ast_of<Ctyp> (type, ctx);
  value_of_context vctx (ctyp->id);
  return ctyp->to_value (vctx);
}


CAMLprim value
clang_type_ptr (value context, value id)
{
  clang_context &ctx = *reinterpret_cast<clang_context *> (Bp_val (context));
  clang_ref<TypeLoc> ref (Unsigned_long_val (id));

  clang::QualType type = ctx.refs.retrieve (ref).getType ();

  ptr<Ctyp> ctyp = bridge_ast_of<Ctyp> (type, ctx);
  value_of_context vctx (ctyp->id);
  return ctyp->to_value (vctx);
}


O_END_DECLS
