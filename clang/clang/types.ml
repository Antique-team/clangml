open Ast


let make_type_map (types : ctyp Util.DenseIntMap.t) =
  let map = Hashtbl.create 10 in

  Util.DenseIntMap.fold (fun key ctyp map ->
    Hashtbl.add map ctyp.t ctyp;
    map
  ) types map



let rec tloc_of_ctyp sloc ty =
  let tl =
    match ty.t with
    | BuiltinType (bt) ->
        BuiltinTypeLoc (bt)
    | TypeOfExprType (expr) ->
        TypeOfExprTypeLoc (expr)
    | TypeOfType (ty) ->
        TypeOfTypeLoc (tloc_of_ctyp sloc ty)
    | ParenType (ty) ->
        ParenTypeLoc (tloc_of_ctyp sloc ty)
    | TypedefType (name) ->
        TypedefTypeLoc (name)
    | PointerType (pointee) ->
        PointerTypeLoc (tloc_of_ctyp sloc pointee)
    | FunctionNoProtoType (retty) ->
        FunctionNoProtoTypeLoc (tloc_of_ctyp sloc retty)
    | FunctionProtoType (retty, args) ->
        (* Make nameless declarations for each argument type. *)
        let args =
          List.map (fun ty ->
              { d = VarDecl (tloc_of_ctyp sloc ty, "", None);
                d_sloc = sloc;
                d_cref = Ref.null;
              }
            ) args
        in
        FunctionProtoTypeLoc (tloc_of_ctyp sloc retty, args)
    | ConstantArrayType (memty, size) ->
        ConstantArrayTypeLoc (tloc_of_ctyp sloc memty, size)
    | VariableArrayType (memty, size) ->
        VariableArrayTypeLoc (tloc_of_ctyp sloc memty, size)
    | IncompleteArrayType (memty) ->
        IncompleteArrayTypeLoc (tloc_of_ctyp sloc memty)
    | ElaboratedType (ty) ->
        ElaboratedTypeLoc (tloc_of_ctyp sloc ty)
    | EnumType name ->
        EnumTypeLoc name
    | RecordType (kind, name) ->
        RecordTypeLoc (kind, name)
    | DecayedType (decayed, original) ->
        (tloc_of_ctyp sloc original).tl
    | TemplateTypeParmType (Some name) ->
        TemplateTypeParmTypeLoc name
    | TemplateTypeParmType None ->
        TemplateTypeParmTypeLoc "<anon>"
    | ComplexType elt -> ComplexTypeLoc elt
    | VectorType (elt_type, num_elts, kind) -> 
        VectorTypeLoc (elt_type, num_elts, kind)


    | AtomicType -> AtomicTypeLoc
    | AttributedType -> AttributedTypeLoc
    | AutoType -> AutoTypeLoc
    | BlockPointerType -> BlockPointerTypeLoc
    | DecltypeType expr -> DecltypeTypeLoc expr
    | DependentNameType -> DependentNameTypeLoc
    | DependentSizedArrayType -> DependentSizedArrayTypeLoc
    | DependentSizedExtVectorType -> DependentSizedExtVectorTypeLoc
    | DependentTemplateSpecializationType -> DependentTemplateSpecializationTypeLoc
    | ExtVectorType -> ExtVectorTypeLoc
    | InjectedClassNameType -> InjectedClassNameTypeLoc
    | LValueReferenceType -> LValueReferenceTypeLoc
    | MemberPointerType -> MemberPointerTypeLoc
    | ObjCInterfaceType -> ObjCInterfaceTypeLoc
    | ObjCObjectPointerType -> ObjCObjectPointerTypeLoc
    | ObjCObjectType -> ObjCObjectTypeLoc
    | PackExpansionType -> PackExpansionTypeLoc
    | RValueReferenceType -> RValueReferenceTypeLoc
    | SubstTemplateTypeParmPackType -> SubstTemplateTypeParmPackTypeLoc
    | SubstTemplateTypeParmType -> SubstTemplateTypeParmTypeLoc
    | TemplateSpecializationType -> TemplateSpecializationTypeLoc
    | UnaryTransformType -> UnaryTransformTypeLoc
    | UnresolvedUsingType -> UnresolvedUsingTypeLoc
  in

  let tl =
    match ty.t_qual, ty.t_aspace with
    | [], None -> tl
    | qual, aspace ->
        QualifiedTypeLoc ({
            tl;
            tl_sloc = sloc;
            tl_cref = Ref.null;
            tl_type = ty;
          }, qual, aspace)
  in

  {
    tl;
    tl_sloc = sloc;
    tl_cref = Ref.null;
    tl_type = ty;
  }
