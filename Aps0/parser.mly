%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR
%token LBRA RBRA
%token TIMES
%token AFFEC
%token ECHO
%token FUN
%token REC
%token CONST
%token SEMICOLON
%token COMMA
%token ARROW_RIGHT
%token INT
%token IF
%token AND
%token OR
%token BOOL

%type <Ast.prog> prog
%type <Ast.cmds> cmds
%type <Ast.def> def
%type <Ast.stat> stat
%type <Ast.exprs> exprs
%type <Ast.expr> expr
%type <Ast.typs> typs
%type <Ast.typ> typ
%type <Ast.args> args
%type <Ast.arg> arg

%start prog

%%
prog: LBRA cmds RBRA    { ASTProg($2) }
;

cmds:
  stat                   { ASTStat($1) }
  |def SEMICOLON cmds     {ASTDef($1, $3)}
;


stat:
  ECHO expr             { ASTEcho($2) }
;

def:
  CONST IDENT typ expr                    {ASTConst($3, $2, $4)}
  |FUN IDENT typ LBRA args RBRA expr      {ASTFun($3, $2, $5, $7)}
  |FUN REC IDENT typ LBRA args RBRA expr  {ASTFunRec($4, $3, $6, $8)}
;

typ :
  BOOL { Bool }
  | INT { Int }
  | LPAR typs ARROW_RIGHT typ RPAR { ASTArrow($2, $4) }
;
typs :
  typ                   {ASTTyp($1)}
  |typ TIMES typs       {ASTTyps($1, $3)}
;

arg : 
  IDENT AFFEC typ    {Arg($1, $3)}
;

args : 
  arg                 {ASTArg($1)}
  | arg COMMA args          {ASTArgs($1, $3)}
;

expr:
  NUM                   { ASTNum($1) }
  | IDENT                 { ASTId($1) }
  | LPAR IF expr expr expr RPAR  {ASTIf($3, $4, $5)}
  | LPAR AND expr expr RPAR  {ASTAnd($3, $4)}
  | LPAR OR expr expr RPAR  {ASTOr($3, $4)}
  | LPAR expr exprs RPAR  { ASTApp($2, $3) }
  | LBRA args RBRA expr {ASTAbs($2,$4)}
;

exprs :
  expr       { ASTExpr($1) }
  | expr exprs { ASTExprs($1, $2) }
;

