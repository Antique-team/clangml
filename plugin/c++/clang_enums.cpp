#include "clang_enums.h"

#include <stdexcept>

using namespace ast_bridge;


UnaryOperator
translate_unary_operator_kind (clang::UnaryOperatorKind kind)
{
  switch (kind)
    {
    case clang::UO_PostInc:	return UO_PostInc;
    case clang::UO_PostDec:	return UO_PostDec;
    case clang::UO_PreInc:	return UO_PreInc;
    case clang::UO_PreDec:	return UO_PreDec;
    case clang::UO_AddrOf:	return UO_AddrOf;
    case clang::UO_Deref:	return UO_Deref;
    case clang::UO_Plus:	return UO_Plus;
    case clang::UO_Minus:	return UO_Minus;
    case clang::UO_Not:		return UO_Not;
    case clang::UO_LNot:	return UO_LNot;
    case clang::UO_Real:	return UO_Real;
    case clang::UO_Imag:	return UO_Imag;
    case clang::UO_Extension:	return UO_Extension;
    }
  throw std::runtime_error ("invalid unary operator kind");
}


BinaryOperator
translate_binary_operator_kind (clang::BinaryOperatorKind kind)
{
  switch (kind)
    {
    case clang::BO_PtrMemD:	return BO_PtrMemD;
    case clang::BO_PtrMemI:	return BO_PtrMemI;
    case clang::BO_Mul:		return BO_Mul;
    case clang::BO_Div:		return BO_Div;
    case clang::BO_Rem:		return BO_Rem;
    case clang::BO_Add:		return BO_Add;
    case clang::BO_Sub:		return BO_Sub;
    case clang::BO_Shl:		return BO_Shl;
    case clang::BO_Shr:		return BO_Shr;
    case clang::BO_LT:		return BO_LT;
    case clang::BO_GT:		return BO_GT;
    case clang::BO_LE:		return BO_LE;
    case clang::BO_GE:		return BO_GE;
    case clang::BO_EQ:		return BO_EQ;
    case clang::BO_NE:		return BO_NE;
    case clang::BO_And:		return BO_And;
    case clang::BO_Xor:		return BO_Xor;
    case clang::BO_Or:		return BO_Or;
    case clang::BO_LAnd:	return BO_LAnd;
    case clang::BO_LOr:		return BO_LOr;
    case clang::BO_Assign:	return BO_Assign;
    case clang::BO_MulAssign:	return BO_MulAssign;
    case clang::BO_DivAssign:	return BO_DivAssign;
    case clang::BO_RemAssign:	return BO_RemAssign;
    case clang::BO_AddAssign:	return BO_AddAssign;
    case clang::BO_SubAssign:	return BO_SubAssign;
    case clang::BO_ShlAssign:	return BO_ShlAssign;
    case clang::BO_ShrAssign:	return BO_ShrAssign;
    case clang::BO_AndAssign:	return BO_AndAssign;
    case clang::BO_OrAssign:	return BO_OrAssign;
    case clang::BO_XorAssign:	return BO_XorAssign;
    case clang::BO_Comma:	return BO_Comma;
    }
  throw std::runtime_error ("invalid binary operator kind");
}


PredefinedExpr
translate_predefined_expr (clang::PredefinedExpr::IdentType kind)
{
  switch (kind)
    {
    case clang::PredefinedExpr::Func:			return PE_Func;
    case clang::PredefinedExpr::Function:		return PE_Function;
    case clang::PredefinedExpr::FuncDName:		return PE_FuncDName;
    case clang::PredefinedExpr::LFunction:		return PE_LFunction;
    case clang::PredefinedExpr::PrettyFunction:		return PE_PrettyFunction;
    case clang::PredefinedExpr::PrettyFunctionNoVirtual:return PE_PrettyFunctionNoVirtual;
    }
  throw std::runtime_error ("invalid predefined expression kind");
}


TagTypeKind
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


VectorKind
translate_vector_kind (clang::VectorType::VectorKind kind)
{
  switch (kind)
    {
    case clang::VectorType::VectorKind::GenericVector:
      return VK_GenericVector;
    case clang::VectorType::VectorKind::AltiVecVector:
      return VK_AltiVecVector;
    case clang::VectorType::VectorKind::AltiVecPixel:
      return VK_AltiVecPixel;
    case clang::VectorType::VectorKind::AltiVecBool:
      return VK_AltiVecBool;
    case clang::VectorType::VectorKind::NeonVector:
      return VK_NeonVector;
    case clang::VectorType::VectorKind::NeonPolyVector:
      return VK_NeonPolyVector;
    }
  throw std::runtime_error ("invalid vector kind");
}


ElaboratedTypeKeyword
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


BuiltinType
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


CastKind
translate_cast_kind (clang::CastKind kind)
{
  switch (kind)
    {
    case clang::CK_Dependent:				return CK_Dependent;
    case clang::CK_BitCast:				return CK_BitCast;
    case clang::CK_LValueBitCast:			return CK_LValueBitCast;
    case clang::CK_LValueToRValue:			return CK_LValueToRValue;
    case clang::CK_NoOp:				return CK_NoOp;
    case clang::CK_BaseToDerived:			return CK_BaseToDerived;
    case clang::CK_DerivedToBase:			return CK_DerivedToBase;
    case clang::CK_UncheckedDerivedToBase:		return CK_UncheckedDerivedToBase;
    case clang::CK_Dynamic:				return CK_Dynamic;
    case clang::CK_ToUnion:				return CK_ToUnion;
    case clang::CK_ArrayToPointerDecay:			return CK_ArrayToPointerDecay;
    case clang::CK_FunctionToPointerDecay:		return CK_FunctionToPointerDecay;
    case clang::CK_NullToPointer:			return CK_NullToPointer;
    case clang::CK_NullToMemberPointer:			return CK_NullToMemberPointer;
    case clang::CK_BaseToDerivedMemberPointer:		return CK_BaseToDerivedMemberPointer;
    case clang::CK_DerivedToBaseMemberPointer:		return CK_DerivedToBaseMemberPointer;
    case clang::CK_MemberPointerToBoolean:		return CK_MemberPointerToBoolean;
    case clang::CK_ReinterpretMemberPointer:		return CK_ReinterpretMemberPointer;
    case clang::CK_UserDefinedConversion:		return CK_UserDefinedConversion;
    case clang::CK_ConstructorConversion:		return CK_ConstructorConversion;
    case clang::CK_IntegralToPointer:			return CK_IntegralToPointer;
    case clang::CK_PointerToIntegral:			return CK_PointerToIntegral;
    case clang::CK_PointerToBoolean:			return CK_PointerToBoolean;
    case clang::CK_ToVoid:				return CK_ToVoid;
    case clang::CK_VectorSplat:				return CK_VectorSplat;
    case clang::CK_IntegralCast:			return CK_IntegralCast;
    case clang::CK_IntegralToBoolean:			return CK_IntegralToBoolean;
    case clang::CK_IntegralToFloating:			return CK_IntegralToFloating;
    case clang::CK_FloatingToIntegral:			return CK_FloatingToIntegral;
    case clang::CK_FloatingToBoolean:			return CK_FloatingToBoolean;
    case clang::CK_FloatingCast:			return CK_FloatingCast;
    case clang::CK_CPointerToObjCPointerCast:		return CK_CPointerToObjCPointerCast;
    case clang::CK_BlockPointerToObjCPointerCast:	return CK_BlockPointerToObjCPointerCast;
    case clang::CK_AnyPointerToBlockPointerCast:	return CK_AnyPointerToBlockPointerCast;
    case clang::CK_ObjCObjectLValueCast:		return CK_ObjCObjectLValueCast;
    case clang::CK_FloatingRealToComplex:		return CK_FloatingRealToComplex;
    case clang::CK_FloatingComplexToReal:		return CK_FloatingComplexToReal;
    case clang::CK_FloatingComplexToBoolean:		return CK_FloatingComplexToBoolean;
    case clang::CK_FloatingComplexCast:			return CK_FloatingComplexCast;
    case clang::CK_FloatingComplexToIntegralComplex:	return CK_FloatingComplexToIntegralComplex;
    case clang::CK_IntegralRealToComplex:		return CK_IntegralRealToComplex;
    case clang::CK_IntegralComplexToReal:		return CK_IntegralComplexToReal;
    case clang::CK_IntegralComplexToBoolean:		return CK_IntegralComplexToBoolean;
    case clang::CK_IntegralComplexCast:			return CK_IntegralComplexCast;
    case clang::CK_IntegralComplexToFloatingComplex:	return CK_IntegralComplexToFloatingComplex;
    case clang::CK_ARCProduceObject:			return CK_ARCProduceObject;
    case clang::CK_ARCConsumeObject:			return CK_ARCConsumeObject;
    case clang::CK_ARCReclaimReturnedObject:		return CK_ARCReclaimReturnedObject;
    case clang::CK_ARCExtendBlockObject:		return CK_ARCExtendBlockObject;
    case clang::CK_AtomicToNonAtomic:			return CK_AtomicToNonAtomic;
    case clang::CK_NonAtomicToAtomic:			return CK_NonAtomicToAtomic;
    case clang::CK_CopyAndAutoreleaseBlockObject:	return CK_CopyAndAutoreleaseBlockObject;
    case clang::CK_BuiltinFnToFnPtr:			return CK_BuiltinFnToFnPtr;
    case clang::CK_ZeroToOCLEvent:			return CK_ZeroToOCLEvent;
    }
  throw std::runtime_error ("invalid cast kind");
}

AccessSpecifier
translate_access_specifier (clang::AccessSpecifier spec)
{
  switch (spec)
    {
    case clang::AS_public:				return AS_public;
    case clang::AS_protected:				return AS_protected;
    case clang::AS_private:				return AS_private;
    case clang::AS_none:				return AS_none;
    }
  throw std::runtime_error ("invalid access specifier");
}


OverloadedOperatorKind
translate_overloaded_operator_kind (clang::OverloadedOperatorKind kind)
{
  switch (kind)
    {
    case clang::OO_None:
      throw std::runtime_error ("invalid overloaded operator kind: OO_None");
#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
    case clang::OO_##Name: return OO_##Name;
#include "clang/Basic/OperatorKinds.def"
    case clang::NUM_OVERLOADED_OPERATORS:
      throw std::runtime_error ("invalid overloaded operator kind: NUM_OVERLOADED_OPERATORS");
    }
  throw std::runtime_error ("invalid overloaded operator kind");
}
