open Clang
open Ast
open C_sig
open C_show
open Util

module Log = Logger.Make(struct let tag = "CtypToMCType" end)


module TypeSet = SparseIntSet.Make(struct

  type t = ctyp DenseIntMap.key

end)


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


let rec c_agg_fields_of_decls clang c_types is_union (decls : decl list) =
  let rec loop off fields = function
    | [] -> List.rev fields

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

        let agg = {
          cag_name   = if name1 = "" then None else Some name1;
          cag_align  = align;
          cag_size   = size;
          cag_fields = c_agg_fields_of_decls clang c_types (kind = TTK_Union) members;
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

        loop off (field :: fields) tl

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

        let field = {
          caf_typ  = DenseIntMap.find ty.tl_type.t_self c_types;
          caf_off  = off;
          caf_size = size;
          caf_name = name;
        } in

        (* offset for the next field *)
        let off = off + size in

        loop off (field :: fields) tl

    | { d } :: tl ->
        print_endline (Show.show<decl_> d);
        Log.err "Only FieldDecls allowed within RecordDecl"
  in

  loop 0 [] decls


(***********************************************************************
 * Typedef
 ***********************************************************************)


and c_type_of_typedef_decl clang seen decl =
  print_endline @@ Show_decl.show decl;
  seen, Ctvoid


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
          seen, Ctptr None
      end


  | ElaboratedType ty ->
      c_type_of_ctyp clang seen ty

  | EnumType name ->
      seen, Ctvoid

  | RecordType (kind, name) ->
      if TypeSet.mem ctyp.t_self seen then
        seen, Ctnamed {
          cnt_name = string_of_int (ctyp.t_self :> int);
          cnt_type = Ctptr None; (* will resolve in second pass *)
        }
      else (
        let seen = TypeSet.add ctyp.t_self seen in

        seen, try
          let decl = Api.(request clang @@ DeclOfType ctyp.t_cref) in

          let size  = sizeof  clang ctyp in
          let align = alignof clang ctyp in

          let c_type =
            make_aggregate {
              cag_name   = if name = "" then None else Some name;
              cag_align  = align;
              cag_size   = size;
              cag_fields = [] (* c_agg_fields_of_decls clang c_types (kind =
                             TTK_Union) members *);
            } kind
          in

          print_endline @@ Show_decl.show decl;
          if name <> "" then
            Ctnamed { cnt_name = name; cnt_type = Ctvoid; }
          else
            Ctvoid
        with Api.E (Api.E_Failure msg) ->
          Log.warn "tag type name: %s: %s" name msg;
          Ctptr None
      )


  | ParenType inner ->
      c_type_of_ctyp clang seen inner

  | PointerType { t = FunctionNoProtoType _
                    | FunctionProtoType _
                }
  | FunctionNoProtoType _
  | FunctionProtoType _ ->
      (* MemCAD ignores the type of functions (and function pointers). *)
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


let map_types clang types =
  let (seen, c_types) =
    DenseIntMap.mapvf
      (fun _i ctyp seen ->
         print_endline "-------------------------------------------------------------";
         print_endline @@ Show_ctyp.show ctyp;
         c_type_of_ctyp clang seen ctyp
      )
      types
      TypeSet.empty
  in

  DenseIntMap.iter
    (fun _ c_type ->
      print_endline @@ Show_c_type.show c_type
    )
    c_types;

  c_types
