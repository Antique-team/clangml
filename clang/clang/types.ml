open Ast
open Util

module Log = Logger.Make(struct let tag = "Types" end)


module Ordered_ctyp : Map.OrderedType
  with type t = AstSimple.ctyp =
struct

  type t = AstSimple.ctyp

  let compare = Pervasives.compare

end


module TypeMap = Map.Make(Ordered_ctyp)


let find_type simple_type map =
  try
    TypeMap.find simple_type map
  with Not_found ->
    []


let make_type_map types =
  DenseIntMap.fold
    (fun key ctyp map ->
       let simplified = AstSimplify.simplify_ctyp ctyp in
       let existing = find_type simplified map in

       TypeMap.add simplified (ctyp :: existing) map
    )
    types
    TypeMap.empty


let find_basic_int_type type_map =
  let candidates =
    find_type AstSimple.(BuiltinType BT_Int) type_map
    |> List.filter (fun ctyp -> ctyp.t_qual = [] && ctyp.t_aspace = None)
  in

  match candidates with
  | [ty] -> ty
  | [] ->
      failwith "No integer builtin type in the program"
  | _ ->
      (* XXX: This should never happen. *)
      failwith "Multiple integer types in the program"


(* Ensures that the type array (DenseIntMap) contains the basic type
   "int", since we need it to synthesise IntegerLiterals. *)
let ensure_basic_int_type types =
  let bt_int = BuiltinType BT_Int in

  let is_basic_int_type ctyp =
    ctyp.t = bt_int &&
    (* These tests are probably never necessary, since the existence
       of e.g. an "int const" (currently, in clang) implies the existence
       of "int". *)
    ctyp.t_qual = [] &&
    ctyp.t_aspace = None
  in

  if DenseIntMap.exists is_basic_int_type types then
    types
  else (
    Log.warn "Synthesising basic unqualified int type";
    let types = (types : (ctyp, ctyp) DenseIntMap.t :> ctyp array) in
    (* Synthesise an int type. *)
    let new_types =
      (* TODO: rewrite this without Obj.magic.
         => encapsulate operations in DenseIntMap module *)
      let count = Array.length types in
      Array.init
        (count + 1)
        (fun i ->
           if i < count then
             types.(i)
           else
             let index : ctyp DenseIntMap.key = Obj.magic count in
             {
               t = bt_int;
               t_cref = Ref.null;
               t_qual = [];
               t_aspace = None;
               t_self = index;
               t_canon = index;
             }
        )
    in
    (Obj.magic new_types : (ctyp, ctyp) DenseIntMap.t)
  )


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
    | ComplexType elt ->
        ComplexTypeLoc elt
    | VectorType (elt_type, num_elts, kind) ->
        VectorTypeLoc (elt_type, num_elts, kind)
    | ExtVectorType (elt_type, num_elts, kind) ->
        ExtVectorTypeLoc (elt_type, num_elts, kind)
    | AttributedType (attributed_type_kind, ctyp) ->
        AttributedTypeLoc (attributed_type_kind, tloc_of_ctyp sloc ctyp, None)
    | AtomicType ctyp ->
        AtomicTypeLoc (tloc_of_ctyp sloc ctyp)
    | ObjCObjectPointerType pointee ->
        ObjCObjectPointerTypeLoc (tloc_of_ctyp sloc pointee)
    | ObjCObjectType base ->
        ObjCObjectTypeLoc (tloc_of_ctyp sloc base)



    | AutoType -> AutoTypeLoc
    | BlockPointerType -> BlockPointerTypeLoc
    | DecltypeType expr -> DecltypeTypeLoc expr
    | DependentNameType -> DependentNameTypeLoc
    | DependentSizedArrayType -> DependentSizedArrayTypeLoc
    | DependentSizedExtVectorType -> DependentSizedExtVectorTypeLoc
    | DependentTemplateSpecializationType -> DependentTemplateSpecializationTypeLoc
    | InjectedClassNameType -> InjectedClassNameTypeLoc
    | LValueReferenceType -> LValueReferenceTypeLoc
    | MemberPointerType -> MemberPointerTypeLoc
    | ObjCInterfaceType -> ObjCInterfaceTypeLoc
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
