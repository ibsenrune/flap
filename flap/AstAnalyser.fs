module AstAnalyser
  open Ast

  let substitute p expr exprIn = 
    match exprIn with
    | CstI(_) as cst -> cst
    | Var(v) when v = p -> expr
    | Var(v) as var -> var
