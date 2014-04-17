#include "ast_bridge_of.h"
#include "clang_context.h"
#include "sloc_bridge.h"

#include <clang/Basic/SourceManager.h>

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
clang_canonical_type (value context, value id)
{
  clang_context &ctx = Context_val (context);
  clang_ref<Ctyp> ref (Unsigned_long_val (id));

  clang::QualType type = ctx.refs.retrieve (ref).getCanonicalType ();

  ptr<Ctyp> ctyp = ast_bridge_of<Ctyp> (type, ctx);

  ctx.values.resize (ctyp->id);
  return ctyp->to_value (ctx.values);
}


CAMLprim value
clang_type_ptr (value context, value id)
{
  clang_context &ctx = Context_val (context);
  clang_ref<Tloc> ref (Unsigned_long_val (id));

  clang::QualType type = ctx.refs.retrieve (ref).getType ();

  ptr<Ctyp> ctyp = ast_bridge_of<Ctyp> (type, ctx);

  ctx.values.resize (ctyp->id);
  return ctyp->to_value (ctx.values);
}


CAMLprim value
clang_presumed_loc (value context, value sloc)
{
  clang_context &ctx = Context_val (context);
  clang::SourceLocation loc = clang::SourceLocation::getFromRawEncoding (Unsigned_int_val (sloc));

  clang::PresumedLoc presumed = ctx.SM.getPresumedLoc (loc);

  ptr<sloc_bridge::PresumedLoc> result = sloc_bridge::mkPresumedLoc ();
  result->loc_filename = presumed.getFilename ();
  result->loc_line = presumed.getLine ();
  result->loc_column = presumed.getColumn ();

  ctx.values.resize (result->id);
  return result->to_value (ctx.values);
}


CAMLprim value
clang_is_from_main_file (value context, value sloc)
{
  clang_context &ctx = Context_val (context);
  clang::SourceLocation loc = clang::SourceLocation::getFromRawEncoding (Unsigned_int_val (sloc));

  return Val_bool (ctx.SM.isFromMainFile (loc));
}


CAMLprim value
clang_characteristic_kind (value context, value sloc)
{
  clang_context &ctx = Context_val (context);
  clang::SourceLocation loc = clang::SourceLocation::getFromRawEncoding (Unsigned_int_val (sloc));

  switch (ctx.SM.getFileCharacteristic (loc))
    {
    case clang::SrcMgr::C_User: return value_of (ctx.values, sloc_bridge::C_User);
    case clang::SrcMgr::C_System: return value_of (ctx.values, sloc_bridge::C_System);
    case clang::SrcMgr::C_ExternCSystem: return value_of (ctx.values, sloc_bridge::C_ExternCSystem);
    }

  failwith ("invalid file characteristic kind");
}


O_END_DECLS
