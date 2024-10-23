(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n' '\r']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | "ECHO"           { ECHO } 
  | "FUN"            { FUN }
  | "REC"            { REC }
  | "CONST"          { CONST }
  | '*'              { TIMES }
  | ':'              { AFFEC }
  | ';'              { SEMICOLON }
  | ','              { COMMA }
  | "->"             { ARROW_RIGHT }
  | "if"             { IF }
  | "and"            { AND }
  | "or"             { OR }
  | "bool"           { BOOL }
  | "int"            { INT }
  (*APS1*)
  | "void"           { VOID }
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "SET"            { SET }
  | "IF"             { IFS } (* if stat *)
  | "WHILE"          { WHILE }
  | "CALL"           { CALL }
      
  | ('-')?['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
