open Ast


let rec tloc_of_ctyp sloc ty =
  let tl =
    match ty.t with
    | UnimpType (name) ->
        UnimpTypeLoc (name)

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
    | EnumType (name) ->
        EnumTypeLoc (name)
    | RecordType (kind, name) ->
        RecordTypeLoc (kind, name)
    | DecayedType (decayed, original) ->
        (tloc_of_ctyp sloc original).tl
  in

  let tl =
    match ty.t_qual, ty.t_aspace with
    | [], None -> tl
    | qual, aspace ->
        QualifiedTypeLoc ({
            tl;
            tl_sloc = sloc;
            tl_cref = Ref.null;
          }, qual, aspace)
  in

  {
    tl;
    tl_sloc = sloc;
    tl_cref = Ref.null;
  }
