module Ast

  type Expr = 
  | CstI of int
  | Var of string
  | Let of string * Expr * Expr
  | LetFun of string * string * Expr * Expr
  | Op of Expr * string * Expr
