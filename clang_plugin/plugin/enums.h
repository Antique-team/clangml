static PredefinedIdent
translate_predefined_ident (clang::PredefinedExpr::IdentType kind)
{
  switch (kind)
    {
    case clang::PredefinedExpr::Func:			return PI_Func;
    case clang::PredefinedExpr::Function:		return PI_Function;
    case clang::PredefinedExpr::LFunction:		return PI_LFunction;
    case clang::PredefinedExpr::FuncDName:		return PI_FuncDName;
    case clang::PredefinedExpr::PrettyFunction:		return PI_PrettyFunction;
    case clang::PredefinedExpr::PrettyFunctionNoVirtual:return PI_PrettyFunctionNoVirtual;
    }
  throw std::runtime_error ("invalid predefined expression kind");
}


static TagTypeKind
translate_tag_type_kind (clang::TagTypeKind kind)
{
  switch (kind)
    {
    case clang::TTK_Struct:	return TTK_Struct;
    case clang::TTK_Interface:	return TTK_Interface;
    case clang::TTK_Union:	return TTK_Union;
    case clang::TTK_Class:	return TTK_Class;
    case clang::TTK_Enum:	return TTK_Enum;
    }
  throw std::runtime_error ("invalid tag type kind");
}


static ElaboratedTypeKeyword
translate_elaborated_type_keyword (clang::ElaboratedTypeKeyword kw)
{
  switch (kw)
    {
    case clang::ETK_Struct:	return ETK_Struct;
    case clang::ETK_Interface:	return ETK_Interface;
    case clang::ETK_Union:	return ETK_Union;
    case clang::ETK_Class:	return ETK_Class;
    case clang::ETK_Enum:	return ETK_Enum;
    case clang::ETK_Typename:	return ETK_Typename;
    case clang::ETK_None:	return ETK_None;
    }
  throw std::runtime_error ("invalid elaborated type keyword");
}


static BuiltinType
translate_builtin_type (clang::BuiltinType::Kind kind)
{
  switch (kind)
    {
#define BUILTIN_TYPE(Id, SingletonId)	\
    case clang::BuiltinType::Id:	\
      return BT_##Id;
#include <clang/AST/BuiltinTypes.def>
    }
  throw std::runtime_error ("invalid builtin type");
}
