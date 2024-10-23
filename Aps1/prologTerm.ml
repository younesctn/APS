(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

let rec print_typ t =
  match t with
    Int -> Printf.printf"int"
    | Bool -> Printf.printf"bool"
    | Void -> Printf.printf"void"
    | ASTArrow(ts, t) -> (
      Printf.printf"arrow([";
      print_typs ts;
      Printf.printf"],";
      print_typ t;
      Printf.printf")"
    )
and print_typs ts =
  match ts with
    ASTTyp t -> print_typ t
    | ASTTyps(t, ts) -> (
      print_typ t;
      print_char ',';
      print_typs ts
    )

let rec print_arg a =
  match a with
    Arg(x, t) -> (
      Printf.printf"(%s," x;
      print_typ t;
      Printf.printf")"
    )
and print_args args =
  match args with
    ASTArg a -> print_arg a
    | ASTArgs(a, args) -> (
      print_arg a;
      print_char ',';
      print_args args
    )
  
let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTApp(e, es) -> (
      Printf.printf"app(";
      print_expr e;
      Printf.printf",[";
      print_exprs es;
      Printf.printf"])"
    )
    | ASTIf(e1, e2, e3) -> (
      Printf.printf"if(";
      print_expr e1;
      Printf.printf",";
      print_expr e2;
      Printf.printf",";
      print_expr e3;
      Printf.printf")"
    )
    | ASTAnd(e1, e2) -> (
      Printf.printf"and(";
      print_expr e1;
      Printf.printf",";
      print_expr e2;
      Printf.printf")"
    )
    | ASTOr(e1, e2) -> (
      Printf.printf"or(";
      print_expr e1;
      Printf.printf",";
      print_expr e2;
      Printf.printf")"
    )
    | ASTAbs(args, e) -> (
      Printf.printf"abs([";
      print_args args;
      Printf.printf"],";
      print_expr e;
      Printf.printf")"
    )
and print_exprs es =
  match es with
    | ASTExpr e -> print_expr e
    | ASTExprs(e, es) -> (
      print_expr e;
      print_char ',';
      print_exprs es
    )

let rec print_cmds cs =
  match cs with
    ASTStat s -> print_stat s
    | ASTDef(d,c) -> (
      Printf.printf("def(");
      print_def d;
      Printf.printf("),");
      print_cmds c
    )
    | ASTStats(s,c) -> (
      print_stat s;
      print_char ',';
      print_cmds c
    )
and print_stat s =
  match s with
    ASTEcho e -> (
      Printf.printf("echo(");
      print_expr(e);
      Printf.printf(")")
    )
    (*APS1*)
    | ASTSet(x, e) -> (
      Printf.printf"set(%s," x;
      print_expr e;
      Printf.printf(")")
    )
    | ASTIfs(e, b1, b2) -> (
      Printf.printf("if1(");
      print_expr e;
      Printf.printf(",");
      print_block b1;
      Printf.printf(",");
      print_block b2;
      Printf.printf(")")
    )
    | ASTWhile(e, b) -> (
      Printf.printf("while(");
      print_expr e;
      Printf.printf(",");
      print_block b;
      Printf.printf(")")
    )
    | ASTCall(s, es) -> (
      Printf.printf"call(%s,[" s;
      print_exprs es;
      Printf.printf("])")
    )
and print_def d =
  match d with
    ASTConst(t, name, e) -> (
      Printf.printf "const(%s," name;
      print_typ t;
      Printf.printf(",");
      print_expr e;
      Printf.printf(")")
    )
    | ASTFun(t, name, args, e) -> (
        Printf.printf"fun(%s," name;
        Printf.printf("[");
        print_args args;
        Printf.printf("],");
        print_expr e;
        Printf.printf(",");
        print_typ t;
        Printf.printf(")")
      )
    | ASTFunRec(t, name, args, e) -> (
        Printf.printf"funrec(%s," name;
        Printf.printf("[");
        print_args args;
        Printf.printf("],");
        print_expr e;
        Printf.printf(",");
        print_typ t;
        Printf.printf(")")
      )
    (*APS1*)
    | ASTVar(t, name) -> (
        Printf.printf"var(%s," name;
        print_typ t;
        Printf.printf(")")
      )
    | ASTProc(name, args, b) -> (
        Printf.printf"proc(%s,[" name;
        print_args args;
        Printf.printf("],");
        print_block b;
        Printf.printf(")")
      )
    | ASTProcRec(name, args, b) -> (
        Printf.printf"procrec(%s,[" name;
        print_args args;
        Printf.printf("],");
        print_block b;
        Printf.printf(")")
      )
(*APS1*)
and print_block b =
  match b with
    ASTBlock cmds -> (
      Printf.printf"block([";
      print_cmds cmds;
      Printf.printf"])"
    )
  
	
let print_prog p =
  match p with
    ASTProg b -> (
      Printf.printf"prog(";
      print_block b;
      Printf.printf")"
    )
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      
