static PredefinedExpr
translate_predefined_expr (clang::PredefinedExpr::IdentType kind)
{
  switch (kind)
    {
    case clang::PredefinedExpr::Func:			return PE_Func;
    case clang::PredefinedExpr::Function:		return PE_Function;
    case clang::PredefinedExpr::LFunction:		return PE_LFunction;
    case clang::PredefinedExpr::FuncDName:		return PE_FuncDName;
    case clang::PredefinedExpr::PrettyFunction:		return PE_PrettyFunction;
    case clang::PredefinedExpr::PrettyFunctionNoVirtual:return PE_PrettyFunctionNoVirtual;
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


static CastKind
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
