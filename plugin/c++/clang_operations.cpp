#include "bridge_ast_of.h"
#include "clang_context.h"

#include <clang/Basic/SourceManager.h>

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
  ctx.values.resize (ctyp->id);
  return ctyp->to_value (ctx.values);
}


CAMLprim value
clang_type_ptr (value context, value id)
{
  clang_context &ctx = *reinterpret_cast<clang_context *> (Bp_val (context));
  clang_ref<Tloc> ref (Unsigned_long_val (id));

  clang::QualType type = ctx.refs.retrieve (ref).getType ();

  ptr<Ctyp> ctyp = bridge_ast_of<Ctyp> (type, ctx);
  ctx.values.resize (ctyp->id);
  return ctyp->to_value (ctx.values);
}


CAMLprim value
clang_presumed_loc (value context, value sloc)
{
  CAMLparam0 ();
  CAMLlocal1 (result);

  clang_context &ctx = *reinterpret_cast<clang_context *> (Bp_val (context));
  clang::SourceLocation loc = clang::SourceLocation::getFromRawEncoding (Unsigned_int_val (sloc));

  clang::PresumedLoc presumed = ctx.SM.getPresumedLoc (loc);

  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_string (presumed.getFilename ()));
  Store_field (result, 1, Val_int (presumed.getLine ()));
  Store_field (result, 2, Val_int (presumed.getColumn ()));

  CAMLreturn (result);
}


O_END_DECLS
