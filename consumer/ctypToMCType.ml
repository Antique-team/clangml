open Clang
open Ast
open C_sig
open C_show
open Util

module Log = Logger.Make(struct let tag = "CtypToMCType" end)


module TypeMap = Map.Make(String)


(***********************************************************************
 * Sizeof/alignof queries
 ***********************************************************************)


let sizeof clang ctyp =
  let open Api in
  request clang @@ SizeofType ctyp.t_cref
  |> Int64.to_int


let alignof clang ctyp =
  let open Api in
  request clang @@ AlignofType ctyp.t_cref


(***********************************************************************
 * Builtin types
 ***********************************************************************)


let c_type_of_builtin_type = function
  | BT_Void -> Ctvoid
  | BT_Char16
  | BT_Char32
  | BT_Short
  | BT_Long
  | BT_LongLong
  | BT_Int128
  | BT_UShort
  | BT_ULong
  | BT_ULongLong
  | BT_UInt128
  | BT_UInt
  | BT_Int -> Ctint
  | BT_Bool
  | BT_SChar
  | BT_UChar
  | BT_Char_S
  | BT_Char_U -> Ctchar
  | _ -> Ctint



(***********************************************************************
 * Aggregates (struct/union)
 ***********************************************************************)


let make_aggregate agg = function
  | TTK_Struct -> Ctstruct agg
  | TTK_Union  -> Ctunion  agg
  | _ -> Log.unimp "Unhandled tag type kind"


let compute_offset is_union off align =
  if false then
    -1
  else
    if is_union then
      0
    else
      ((off + align - 1) / align) * align


let rec c_agg_fields_of_decls clang seen is_union (decls : decl list) =
  let rec loop off (seen, fields) = function
    | [] -> (seen, List.rev fields)

    | { d = RecordDecl (_, name1, Some members, _) }
      :: { d = FieldDecl {
            fd_type = {
              tl = ElaboratedTypeLoc {
                tl = RecordTypeLoc (kind, name2);
                tl_type;
              }
            };
            fd_name = name;
            fd_bitw = bitwidth;
            fd_init = init;
        } } :: tl
      when name1 = name2 ->
        if bitwidth <> None then
          Log.unimp "Bit fields not implemented";
        if init <> None then
          Log.unimp "Member initialisers not implemented";

        let size  = sizeof  clang tl_type in
        let align = alignof clang tl_type in

        let seen, cag_fields =
          c_agg_fields_of_decls clang seen (kind = TTK_Union) members
        in

        let agg = {
          cag_name   = if name1 = "" then None else Some name1;
          cag_align  = align;
          cag_size   = size;
          cag_fields;
        } in

        (* offset for the current field *)
        let off = compute_offset is_union off align in

        let field = {
          caf_typ  = make_aggregate agg kind;
          caf_off  = off;
          caf_size = size;
          caf_name = name;
        } in

        (* offset for the next field *)
        let off = off + size in

        loop off (seen, field :: fields) tl

    | { d = FieldDecl { fd_type = ty;
                        fd_name = name;
                        fd_bitw = bitwidth;
                        fd_init = init;
                        fd_index;
                      } } :: tl ->
        if bitwidth <> None then
          Log.unimp "Bit fields not implemented";
        if init <> None then
          Log.unimp "Member initialisers not implemented";

        let size  = sizeof  clang ty.tl_type in
        let align = alignof clang ty.tl_type in

        (* offset for the current field *)
        let off = compute_offset is_union off align in

        let seen, ty = c_type_of_ctyp clang seen ty.tl_type in

        let field = {
          caf_typ  = ty;
          caf_off  = off;
          caf_size = size;
          caf_name = name;
        } in

        (* offset for the next field *)
        let off = off + size in

        loop off (seen, field :: fields) tl

    | { d } :: tl ->
        print_endline (Show.show<decl_> d);
        Log.err "Only FieldDecls allowed within RecordDecl"
  in

  loop 0 (seen, []) decls


(***********************************************************************
 * Typedef
 ***********************************************************************)


and c_type_of_typedef_decl clang seen decl =
  print_endline @@ Show_decl.show decl;
  let tloc = Query.underlying_type_of_typedef_decl decl.d in
  c_type_of_ctyp clang seen tloc.tl_type


(***********************************************************************
 * Main recursive entry point for type translation
 ***********************************************************************)


(* clang type (ctyp) to MemCAD C type (c_type) *)
and c_type_of_ctyp clang seen ctyp =
  match ctyp.t with
  | BuiltinType bt ->
      seen, c_type_of_builtin_type bt

  | ConstantArrayType (memty, size) ->
      let (seen, memty) = c_type_of_ctyp clang seen memty in
      seen, Ctarray (memty, size)

  | TypedefType name ->
      assert (name <> "");
      begin
        try
          let decl = Api.(request clang @@ DeclOfType ctyp.t_cref) in
          let (seen, c_type) = c_type_of_typedef_decl clang seen decl in
          (* assert? seen was not changed *)
          seen, Ctnamed { cnt_name = name; cnt_type = c_type; }
        with Api.E (Api.E_Failure msg) ->
          Log.warn "typedef name: %s: %s" name msg;
          (seen, Ctnamed { cnt_name = name; cnt_type = Ctvoid; })
      end


  | ElaboratedType ty ->
      c_type_of_ctyp clang seen ty

  | EnumType name ->
      (* not implemented in MemCAD *)
      Log.unimp "EnumType name: %s" name

  | RecordType (kind, name) ->
      let cnt_name = string_of_int (ctyp.t_self :> int) in
      if TypeMap.mem cnt_name seen then
        seen, Ctnamed {
          cnt_name = name;
          cnt_type =
            Ctnamed {
              cnt_name;
              cnt_type = Ctptr None; (* will resolve in second pass *)
            };
        }
      else (
        let seen = TypeMap.add cnt_name ctyp.t_self seen in

        try
          let decl = Api.(request clang @@ DeclOfType ctyp.t_cref) in

          let size  = sizeof  clang ctyp in
          let align = alignof clang ctyp in

          let seen, cag_fields =
            c_agg_fields_of_decls clang seen (kind = TTK_Union)
              (Query.fields_of_record_decl decl.d)
          in

          let c_type =
            make_aggregate {
              cag_name   = if name = "" then None else Some name;
              cag_align  = align;
              cag_size   = size;
              cag_fields;
            } kind
          in

          (seen, c_type)
        with Api.E (Api.E_Failure msg) ->
          Log.warn "tag type name: %s: %s" name msg;
          (seen, Ctnamed { cnt_name = "struct " ^ name; cnt_type = Ctvoid; })
      )


  | ParenType inner ->
      c_type_of_ctyp clang seen inner

  | PointerType { t = FunctionNoProtoType _
                    | FunctionProtoType _
                }
  | FunctionNoProtoType _
  | FunctionProtoType _ ->
      (* MemCAD ignores the type of functions (and function pointers)
         so Ctvoid is correct here *)
      seen, Ctvoid

  | PointerType pointee ->
      let (seen, pointee) = c_type_of_ctyp clang seen pointee in
      seen, Ctptr (Some (pointee))

  | TypeOfExprType _ -> Log.unimp "TypeOfExprType"
  | TypeOfType _ -> Log.unimp "TypeOfType"
  | VariableArrayType _ -> Log.unimp "VariableArrayType"
  | IncompleteArrayType _ -> Log.unimp "IncompleteArrayType"
  | DecayedType _ -> Log.unimp "IncompleteArrayType"

  | ty -> Log.unimp "%a" Show.format<ctyp_> ty


(***********************************************************************
 * Mapping the type map (array) from clang to memcad types
 ***********************************************************************)

let rec resolve_c_aggregate seen c_types aggr =
  List.iter
    (fun cf ->
       resolve_c_type seen c_types cf.caf_typ
    )
    aggr.cag_fields

and resolve_c_type seen c_types = function
  | Ctint
  | Ctchar
  | Ctvoid
  | Ctptr None -> ()

  | Ctnamed ({ cnt_type = Ctnamed { cnt_name; cnt_type = Ctptr None; } } as c_named) ->
      let key = TypeMap.find cnt_name seen in
      c_named.cnt_type <- DenseIntMap.find key c_types

  | Ctnamed c_named ->
      resolve_c_type seen c_types c_named.cnt_type

  | Ctstruct aggr -> resolve_c_aggregate seen c_types aggr
  | Ctunion  aggr -> resolve_c_aggregate seen c_types aggr

  | Ctptr (Some ty)
  | Ctarray (ty, _) ->
      resolve_c_type seen c_types ty


let map_types clang types =
  let (seen, c_types) =
    DenseIntMap.mapvf
      (fun _i ctyp seen ->
         c_type_of_ctyp clang seen ctyp
      )
      types
      TypeMap.empty
  in

  DenseIntMap.iter
    (fun _ c_type ->
       resolve_c_type seen c_types c_type
    )
    c_types;

  (*
  DenseIntMap.iter
    (fun key c_type ->
       Format.printf "%d: %a\n"
         (key : ctyp DenseIntMap.key :> int)
         Show_c_type.format c_type
    )
    c_types;
  *)

  c_types
