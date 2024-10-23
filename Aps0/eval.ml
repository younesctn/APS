(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: eval.ml                                                     == *)
(* ==  Evaluation                                                          == *)
(* ========================================================================== *)
open Ast;;

type values =
  InZ of int
  | InF of expr * string list * (string * values) list
  | InFR of expr * string * string list * (string * values) list

let eval_prim1 op v =
  match op, v with
    "not", InZ n -> InZ (if n = 0 then 1 else 0)
    | _ -> failwith "Unknown unary operator"

let eval_prim2 op v1 v2 =
  match op, v1, v2 with
    "add", InZ n1, InZ n2 -> InZ (n1 + n2)
    | "sub", InZ n1, InZ n2 -> InZ (n1 - n2)
    | "mul", InZ n1, InZ n2 -> InZ (n1 * n2)
    | "div", InZ n1, InZ n2 -> InZ (n1 / n2)
    | "eq", InZ n1, InZ n2 -> InZ (if n1 = n2 then 1 else 0)
    | "lt", InZ n1, InZ n2 -> InZ (if n1 < n2 then 1 else 0)
    | _ -> failwith "Unknown binary operator"

let get_id_of_arg (arg : Ast.arg) =
  match arg with
    Arg(id,t) -> id
;;

let rec get_arg_value l a =
  match l with
    [] -> failwith "Unknown argument"
    | (id, v)::tl -> if id = a then v else get_arg_value tl a
;;

let rec id_of_args (args : Ast.args) =
  match args with
    ASTArg a -> (get_id_of_arg a)::[]
    | ASTArgs(a1,a2) -> (get_id_of_arg a1)::(id_of_args a2)
;;

(*utilisation de cette fonction recommandee par des camarades*)
let rec add_to_env ids vals env =
  match ids, vals with
    [], [] -> env
    | id::tl1, v::tl2 -> (id, v)::(add_to_env tl1 tl2 env)
    | _, _ -> failwith "Different number of arguments and values"
;;

let rec eval_expr expr env =
  match expr with
    ASTNum n -> InZ n
  | ASTId x -> ( match x with
      "true" -> InZ 1
      | "false" -> InZ 0
      | _ -> get_arg_value env x
    )
  | ASTIf(e1, e2, e3) -> (
        match eval_expr e1 env with
          InZ 0 -> eval_expr e3 env
          | _ -> eval_expr e2 env
        )
  | ASTAnd(e1, e2) -> (
        match (eval_expr e1 env, eval_expr e2 env) with
          (InZ 0, _) -> InZ 0
          | (_, InZ 0) -> InZ 0
          | _ -> InZ 1
        )
  | ASTOr(e1, e2) -> (
        match (eval_expr e1 env, eval_expr e2 env) with
          (InZ 1, _) -> InZ 1
          | (_, InZ 1) -> InZ 1
          | _ -> InZ 0
        )
  | ASTAbs(a, e) -> InF(e, id_of_args a, env)
  | ASTApp(e, es) -> (
        match e with
          ASTId("not") -> let vals = eval_exprs es env in eval_prim1 "not" (List.hd vals)
          | ASTId("add") -> let vals = eval_exprs es env in eval_prim2 "add" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("sub") -> let vals = eval_exprs es env in eval_prim2 "sub" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("mul") -> let vals = eval_exprs es env in eval_prim2 "mul" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("div") -> let vals = eval_exprs es env in eval_prim2 "div" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("eq") -> let vals = eval_exprs es env in eval_prim2 "eq" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("lt") -> let vals = eval_exprs es env in eval_prim2 "lt" (List.hd vals) (List.hd (List.tl vals))
          | _ -> ( match eval_expr e env with
            InZ _ -> failwith "Application of a non-function"
            | InF(e, a, envf) -> let new_env = add_to_env a (eval_exprs es env) envf in eval_expr e new_env
            | InFR(e, name, a, envf) -> let new_env = (name, InFR(e, name, a, envf))::(add_to_env a (eval_exprs es env) envf) in eval_expr e new_env
          )
        )
and eval_exprs es env =
  match es with
    ASTExpr e -> (eval_expr e env)::[]
    | ASTExprs(e, es) -> (eval_expr e env)::(eval_exprs es env)

let eval_stat stat env w =
  match stat with
    ASTEcho e -> (eval_expr e env)::w

let eval_def def env =
  match def with
    ASTConst(t, x, e) -> (x, eval_expr e env)::env
    | ASTFun(t, f, a, e) -> (f, InF (e, id_of_args a, env))::env
    | ASTFunRec(t, f, a, e) -> (f, InFR (e, f, id_of_args a, env))::env

let rec eval_cmds cmds env w =
  match cmds with
    | ASTStat stat -> eval_stat stat env w
    | ASTDef(def,cs) -> eval_cmds cs (eval_def def env) w 

let eval_prog prog =
  match prog with
    ASTProg cmds -> eval_cmds cmds [] []

let rec print_result (res : values list) =
  match res with
    [] -> ()
    | InZ n::tl -> print_int n; print_newline (); print_result tl
    | _ -> failwith "Unknown value"
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    print_result (eval_prog p);
  with Lexer.Eof ->
    exit 0