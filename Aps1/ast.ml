(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type typ =
  Bool
  | Int
  | Void
  | ASTArrow of typs * typ
and typs =
  ASTTyp of typ
  | ASTTyps of typ * typs

type arg =
  | Arg of string * typ
and args =
  | ASTArg of arg
  | ASTArgs of arg * args

type expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * exprs
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTAbs of args * expr
and exprs =
  ASTExpr of expr
  | ASTExprs of expr * exprs

type cmds =
  ASTStat of stat
  | ASTDef of def * cmds
  (*APS1*)
  | ASTStats of stat * cmds
and stat =
  ASTEcho of expr
  (*APS1*)
  | ASTSet of string * expr
  | ASTIfs of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * exprs
and def =
  ASTConst of typ * string * expr
  | ASTFun of typ * string * args * expr
  | ASTFunRec of typ * string * args * expr
  (*APS1*)
  | ASTVar of typ * string
  | ASTProc of string * args * block
  | ASTProcRec of string * args * block
and block =
  ASTBlock of cmds

type prog =
  ASTProg of block
