open Camlp4.PreCast

module M = Camlp4OCamlRevisedParser.Make(Syntax)
module P4Parser = Camlp4OCamlParser.Make(Syntax)
(* DCC: Note: above are needed for their side effects (grrr) *)

module Quotation = Camlp4QuotationCommon.Make(P4Parser)(Syntax.AntiquotSyntax)
module P4Printer = Camlp4.Printers.OCaml.Make(Syntax)

module OCamlPrinter = Printers.OCaml
module OCamlDumper  = Printers.DumpOCamlAst


(* Debugging *)

let print_meta_expr (e : Ast.expr) : unit =
  P4Printer.print None (fun o -> o#expr) e;
  print_endline ""

let print_str_item_as_meta (str_item : Ast.str_item) : unit =
  let meta_e = Quotation.MetaAst.Expr.meta_str_item Ast.Loc.ghost str_item in
  print_meta_expr meta_e

let print_ctyp (t : Ast.ctyp) : unit =
  P4Printer.print None (fun o f t -> Format.fprintf f "@[<v2>ctyp: <%a>@]@\n" o#ctyp t) t

let print_ctyp_as_meta (t : Ast.ctyp) : unit =
  let meta_t = Quotation.MetaAst.Expr.meta_ctyp Ast.Loc.ghost t in
  print_meta_expr meta_t

let print_expanded_str_item str_item =
  let e = Quotation.MetaAst.Expr.meta_str_item Ast.Loc.ghost str_item in
  print_meta_expr e
