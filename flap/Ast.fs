module Ast

  type Expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * Expr * Expr
  | LetFun of string * string * Expr * Expr
  | Op of Expr * string * Expr
  | Call of string * Expr
  | If of Expr * Expr * Expr
