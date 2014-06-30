#include "ast_bridge_of.h"
#include "clang_context.h"
#include "sloc_bridge.h"

#include <clang/AST/ASTContext.h>

using namespace ast_bridge;

O_BEGIN_DECLS

static clang_context &
Context_val (value context)
{
  return *reinterpret_cast<clang_context *> (Bp_val (context));
}


CAMLprim value
check_ast_bridge_version (value version)
{
  if (strcmp (String_val (version), ast_bridge::version) != 0)
    {
      char buf[128];
      snprintf (buf, sizeof buf,
                "Ast bridge version mismatch: OCaml side is \"%s\", C++ side is \"%s\"",
                String_val (version), ast_bridge::version);
      failwith (buf);
    }
  return Val_unit;
}


CAMLprim value
clang_canonical_type (value context, value tloc)
{
  clang_context &ctx = Context_val (context);
  clang_ref<Ctyp> ref (Unsigned_long_val (tloc));

  clang::QualType const &type = ctx.refs.retrieve (ref).getCanonicalType ();

  ptr<Ctyp> ctyp = ast_bridge_of<Ctyp> (type, ctx);

  ctx.values.resize (ctyp);
  return ctx.values.to_value (ctyp);
}


CAMLprim value
clang_type_sizeof (value context, value ctyp)
{
  clang_context &ctx = Context_val (context);
  clang_ref<Ctyp> ref (Unsigned_long_val (ctyp));

  clang::QualType const &type = ctx.refs.retrieve (ref);

  int64 size = 0;
  if (!type->isIncompleteType ())
    size = ctx->getTypeInfo (type).first / CHAR_BIT;

  return caml_copy_int64 (size);
}


CAMLprim value
clang_type_alignof (value context, value ctyp)
{
  clang_context &ctx = Context_val (context);
  clang_ref<Ctyp> ref (Unsigned_long_val (ctyp));

  clang::QualType const &type = ctx.refs.retrieve (ref);

  int align = 0;
  if (!type->isIncompleteType ())
    align = ctx->getTypeInfo (type).second / CHAR_BIT;

  return Val_int (align);
}


CAMLprim value
clang_type_ptr (value context, value tloc)
{
  clang_context &ctx = Context_val (context);
  clang_ref<Tloc> ref (Unsigned_long_val (tloc));

  clang::QualType const &type = ctx.refs.retrieve (ref).getType ();

  ptr<Ctyp> ctyp = ast_bridge_of<Ctyp> (type, ctx);

  ctx.values.resize (ctyp);
  return ctx.values.to_value (ctyp);
}


CAMLprim value
clang_presumed_loc (value context, value sloc)
{
  clang_context &ctx = Context_val (context);
  clang::SourceLocation loc = clang::SourceLocation::getFromRawEncoding (Int32_val (sloc));

  clang::PresumedLoc presumed = ctx->getSourceManager ().getPresumedLoc (loc);

  ptr<sloc_bridge::PresumedLoc> result = sloc_bridge::mkPresumedLoc ();
  result->loc_filename = presumed.getFilename ();
  result->loc_line = presumed.getLine ();
  result->loc_column = presumed.getColumn ();

  ctx.values.resize (result);
  return ctx.values.to_value (result);
}


CAMLprim value
clang_is_from_main_file (value context, value sloc)
{
  clang_context &ctx = Context_val (context);
  clang::SourceLocation loc = clang::SourceLocation::getFromRawEncoding (Int32_val (sloc));

  return Val_bool (ctx->getSourceManager ().isInMainFile (loc));
}


CAMLprim value
clang_characteristic_kind (value context, value sloc)
{
  clang_context &ctx = Context_val (context);
  clang::SourceLocation loc = clang::SourceLocation::getFromRawEncoding (Int32_val (sloc));

  switch (ctx->getSourceManager ().getFileCharacteristic (loc))
    {
    case clang::SrcMgr::C_User: return value_of (ctx.values, sloc_bridge::C_User);
    case clang::SrcMgr::C_System: return value_of (ctx.values, sloc_bridge::C_System);
    case clang::SrcMgr::C_ExternCSystem: return value_of (ctx.values, sloc_bridge::C_ExternCSystem);
    }

  failwith ("invalid file characteristic kind");
}


CAMLprim value
clang_cache_for_ctyp (value context)
{
  clang_context &ctx = Context_val (context);

  return ctx.values.get<Ctyp> ();
}


O_END_DECLS
