type ast_node =
  | Decl of ClangBridge.decl
  | Expr of ClangBridge.expr
  | Stmt of ClangBridge.stmt
  | Type of ClangBridge.ctyp
  | TypeLoc of ClangBridge.type_loc

type message =
  | List of message list
  | AstNode of ast_node
  | Filename of string


let recv () : message =
  Marshal.from_channel stdin
