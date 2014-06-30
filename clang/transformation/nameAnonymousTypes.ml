open Clang
open Util.Prelude


let transform_decl clang =
  let open Ast in

  let rec map_decl v state decl =
    match decl.d with
    | RecordDecl (kind, name, Some members, bases) ->
        let members = map_members v [] members in
        state, {
          decl with
          d = RecordDecl (kind, name, Some members, bases)
        }

    | _ ->
        MapVisitor.visit_decl v state decl


  and map_members v state = function
    | [] -> List.rev state
    | ({ d = RecordDecl (kind, "", members, bases) } as rdecl)
      :: ({ d = FieldDecl ({ fd_type = ({
          tl = ElaboratedTypeLoc ({
              tl = RecordTypeLoc (kind2, "");
            } as tl) } as fd_type) } as field) } as fdecl)
      :: rest ->
      let state =
        {
          fdecl with
          d = FieldDecl {
              field with
              fd_type = {
                fd_type with
                tl = ElaboratedTypeLoc {
                    tl with
                    tl = RecordTypeLoc (kind2, "anon");
                  };
              };
            };
        }
        :: { rdecl with d = RecordDecl (kind, "anon", members, bases) }
        :: state
      in
      map_members v state rest

    | hd :: tl ->
        map_members v (hd :: state) tl

  in

  let v = MapVisitor.({ default with map_decl }) in

  snd % MapVisitor.visit_decl v ()
