module Ast

  type Expr = 
  | CstI of int
  | Var of string
  | Op of Expr * string * Expr
