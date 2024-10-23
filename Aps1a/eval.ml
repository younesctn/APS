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
  (*APS1*)
  | InA of int
  | InP of block * string list * (string * values) list
  | InPR of block * string * string list * (string * values) list
  | Smth

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

(*on peut aussi passer la memoire en parametre*)
let rec add_to_env ids vals env =
  match ids, vals with
    [], [] -> env
    | id::tl1, v::tl2 -> (id, v)::(add_to_env tl1 tl2 env)
    | _, _ -> failwith "Different number of arguments and values"
;;

let get_id_of_arg (arg : Ast.arg) =
  match arg with
    Arg(id,t) -> id
;;

let rec id_of_args (args : Ast.args) =
  match args with
    ASTArg a -> (get_id_of_arg a)::[]
    | ASTArgs(a1,a2) -> (get_id_of_arg a1)::(id_of_args a2)
;;

let rec get_arg_value l a =
  match l with
    [] -> failwith "Unknown argument"
    | (id, v)::tl -> if id = a then v else get_arg_value tl a
;;

let get_if_of_argp (arg : Ast.argp) =
  match arg with
    Argp(id,t) -> id
    | ArgpVar(id,t) -> id
;;

let rec id_of_argps (args : Ast.argps) =
  match args with
    ASTArgp a -> (get_if_of_argp a)::[]
    | ASTArgps(a1,a2) -> (get_if_of_argp a1)::(id_of_argps a2)
;;

let rec set_memory adr v mem =
  match mem with
    [] -> failwith "Null pointer exception"
    | (addr, value)::tl -> if addr = adr then (adr, v)::tl else (addr, value)::(set_memory adr v tl)
;;

let rec eval_expr expr env mem =
  match expr with
    ASTNum n -> InZ n
    | ASTId x -> ( match x with
      "true" -> InZ 1
      | "false" -> InZ 0
      | _ -> match (get_arg_value env x) with
          InA a -> List.assoc a mem (*APS1 - cas ou x est une adresse*)
          | _ -> (get_arg_value env x)
        )
  | ASTIf(e1, e2, e3) -> (
        match eval_expr e1 env mem with
          InZ 0 -> eval_expr e3 env mem
          | _ -> eval_expr e2 env mem
        )
  | ASTAnd(e1, e2) -> (
        match (eval_expr e1 env mem, eval_expr e2 env mem) with
          (InZ 0, _) -> InZ 0
          | (_, InZ 0) -> InZ 0
          | _ -> InZ 1
        )
  | ASTOr(e1, e2) -> (
        match (eval_expr e1 env mem, eval_expr e2 env mem) with
          (InZ 1, _) -> InZ 1
          | (_, InZ 1) -> InZ 1
          | _ -> InZ 0
        )
  | ASTAbs(a, e) -> InF(e, id_of_args a, env)
  | ASTApp(e, es) -> (
        match e with
          ASTId("not") -> let vals = eval_exprs es env mem in eval_prim1 "not" (List.hd vals)
          | ASTId("add") -> let vals = eval_exprs es env mem in eval_prim2 "add" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("sub") -> let vals = eval_exprs es env mem in eval_prim2 "sub" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("mul") -> let vals = eval_exprs es env mem in eval_prim2 "mul" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("div") -> let vals = eval_exprs es env mem in eval_prim2 "div" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("eq") -> let vals = eval_exprs es env mem in eval_prim2 "eq" (List.hd vals) (List.hd (List.tl vals))
          | ASTId("lt") -> let vals = eval_exprs es env mem in eval_prim2 "lt" (List.hd vals) (List.hd (List.tl vals))
          | _ -> (
              match eval_expr e env mem with
                | InF(e, a, envf) -> let new_env = add_to_env a (eval_exprs es env mem) envf in eval_expr e new_env mem
                | InFR(e, name, a, envf) -> let new_env = (name, InFR(e, name, a, envf))::(add_to_env a (eval_exprs es env mem) envf) in eval_expr e new_env mem
                | _ -> failwith "Unknown function"
          )
        )
and eval_exprs es env mem =
  match es with
    ASTExpr e -> (eval_expr e env mem)::[]
    | ASTExprs(e, es) -> (eval_expr e env mem)::(eval_exprs es env mem)

let rec eval_exprp es env mem =
  match es with
    Exprp e -> eval_expr e env mem
    | ExprpAdr s -> List.assoc s env
and eval_exprps es env mem =
  match es with
    ASTExprp e -> (eval_exprp e env mem)::[]
    | ASTExprps(e, es) -> (eval_exprp e env mem)::(eval_exprps es env mem)

and eval_stat stat env w mem =
  match stat with
    ASTEcho e -> let w1 = (eval_expr e env mem)::w in (mem, w1)
    | ASTSet(x, e) -> (let adr = List.assoc x env in 
      match adr with
        InA a -> let v = eval_expr e env mem in let mem1 = set_memory a v mem in (mem1, w)
        | _ -> failwith "Valeur non initialisée")
    | ASTIfs(e, b1, b2) -> (
        match eval_expr e env mem with
          InZ 0 -> (eval_block b2 env w mem)
          | _ -> (eval_block b1 env w mem)
        )
    | ASTWhile(e, b) -> (
        match eval_expr e env mem with
          InZ 0 -> (mem, w)
          | _ -> let (mem1, w1) = eval_block b env w mem in eval_stat stat env w1 mem1
        )
    | ASTCall(x, es) -> (
        let v = List.assoc x env in
        match v with
          InP(b, args, envp) -> (
            let vs = eval_exprps es env mem in
            let new_env = add_to_env args vs env in
            eval_block b new_env w mem
          )
          | InPR(b, name, args, envp) -> (
            let vs = eval_exprps es env mem in
            let new_vals = InPR(b, name, args, envp)::vs in
            let new_args = name::args in 
            let new_env = add_to_env new_args new_vals env in
            eval_block b new_env w mem  
          )
          | _ -> failwith "Unknown procedure"
        )


and eval_def def env mem =
  match def with
    ASTConst(t, x, e) -> (
      match eval_expr e env mem with
        | valeur -> (add_to_env [x] [valeur] env, mem)
    )
    | ASTFun(t, f, a, e) -> ((f, InF (e, id_of_args a, env))::env, mem)
    | ASTFunRec(t, f, a, e) -> ((f, InFR (e, f, id_of_args a, env))::env, mem)
    (*APS1*)
    | ASTVar(t, x) -> let addr = List.length mem in 
      let new_env = add_to_env [x] [InA addr] env in
      let new_mem = add_to_env [addr] [Smth] mem in (new_env, new_mem)
    | ASTProc(name, args, b) -> ((name, InP(b, id_of_argps args, env))::env, mem)
    | ASTProcRec(name, args, b) -> ((name, InPR(b, name, id_of_argps args, env))::env, mem)
    

and eval_cmds cmds env w mem =
  match cmds with
    | ASTStat stat -> eval_stat stat env w mem
    | ASTDef(def,cs) -> let (env1, mem1) = eval_def def env mem in eval_cmds cs env1 w mem1
    (*APS1*)
    | ASTStats(stat,cs) -> let (mem1, w1) = eval_stat stat env w mem in eval_cmds cs env w1 mem1

(*APS1*)
and eval_block block env w mem =
  match block with
    ASTBlock cmds -> eval_cmds cmds env w mem

and eval_prog prog =
  match prog with
    ASTProg b -> eval_block b [] [] []

let rec print_result (res : values list) =
  match res with
    [] -> ()
    | InZ n::tl -> print_result tl; print_int n ; print_newline ()
    | _ -> failwith "Unknown value"
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    let env,w = eval_prog p in
    print_result w;
  with Lexer.Eof ->
    exit 0