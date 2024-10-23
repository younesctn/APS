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
/*APS1*/
%token VAR PROC SET IFS WHILE CALL VOID
/*APS1a*/
%token VARP ADR

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
/*APS1*/
%type <Ast.block> block
/*APS1a*/
%type <Ast.exprp> exprp
%type <Ast.exprps> exprps
%type <Ast.argp > argp
%type <Ast.argps> argps


%start prog

%%
prog: block    { ASTProg($1) }
;

/*APS1*/
block: LBRA cmds RBRA    { ASTBlock($2) }

cmds:
  stat                   { ASTStat($1) }
  |def SEMICOLON cmds     {ASTDef($1, $3)}
  /*APS1*/
  |stat SEMICOLON cmds    {ASTStats($1, $3)}
;


stat:
  ECHO expr             { ASTEcho($2) }
  /*APS1*/
  | SET IDENT expr      { ASTSet($2, $3) }
  | IFS expr block block { ASTIfs($2, $3, $4) }
  | WHILE expr block    { ASTWhile($2, $3) }
  | CALL IDENT exprps    { ASTCall($2, $3) }
;

def:
  CONST IDENT typ expr                    {ASTConst($3, $2, $4)}
  | FUN IDENT typ LBRA args RBRA expr      {ASTFun($3, $2, $5, $7)}
  | FUN REC IDENT typ LBRA args RBRA expr  {ASTFunRec($4, $3, $6, $8)}
  /*APS1*/
  | VAR IDENT typ                          {ASTVar($3, $2)}
  /*APS1a*/
  | PROC IDENT LBRA argps RBRA block        {ASTProc($2, $4, $6)}
  | PROC REC IDENT LBRA argps RBRA block    {ASTProcRec($3, $5, $7)}
;

typ :
  BOOL { Bool }
  | INT { Int }
  | VOID { Void }
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

argp : 
  IDENT AFFEC typ    {Argp($1, $3)}
  | VARP IDENT AFFEC typ {ArgpVar($2, $4)}
;

argps : 
  argp                 {ASTArgp($1)}
  | argp COMMA argps          {ASTArgps($1, $3)}
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

/*APS1a*/
exprp:
  expr      { Exprp($1) }
  | LPAR ADR IDENT RPAR {ExprpAdr($3)}
;

exprps :
  exprp       { ASTExprp($1) }
  | exprp exprps { ASTExprps($1, $2) }
;

