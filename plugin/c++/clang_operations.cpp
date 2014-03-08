#include "clang_context.h"
#include "OCamlVisitor.h"

using namespace bridge_ast;

O_BEGIN_DECLS


CAMLprim value
check_bridge_version (value version)
{
  if (strcmp (String_val (version), bridge_ast::version) != 0)
    {
      char buf[128];
      snprintf (buf, sizeof buf,
                "Bridge version mismatch: OCaml side is \"%s\", C++ side is \"%s\"",
                String_val (version), bridge_ast::version);
      failwith (buf);
    }
  return Val_unit;
}


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
