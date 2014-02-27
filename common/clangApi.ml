type ast_node =
  | Decl of ClangAst.decl
  | Expr of ClangAst.expr
  | Stmt of ClangAst.stmt
  | Type of ClangAst.ctyp
  | TypeLoc of ClangAst.type_loc

type message =
  | List of message list
  | AstNode of ast_node
  | Filename of string


let recv () : message =
  Marshal.from_channel stdin
