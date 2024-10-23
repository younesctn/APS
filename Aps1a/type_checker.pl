% contexte de typage initial
gamma_0([
    (true, bool),
    (false, bool),
    (not, arrow([bool],bool)),
    (eq, arrow([int, int], bool)),
    (lt, arrow([int, int], bool)),
    (add, arrow([int, int], int)),
    (sub, arrow([int, int], int)),
    (mul, arrow([int, int], int)),
    (div, arrow([int, int], int))
]).


% Predicate to print each element of the list on a new line
print_list([]). % Base case: if the list is empty, do nothing.
print_list([Head|Tail]) :-
    writeln(Head),
    print_list(Tail).
    
% Base case: Empty list results in an empty list of types
recolt_args_lists([], []).

% Recursive case: Process each (Variable:Type) pair
recolt_args_lists([(_, Type)|RestArgs], [Type|ReturnTypes]) :-
    recolt_args_lists(RestArgs, ReturnTypes).

recolt_args_lists([(varp(_),Type)|RestArgs],[ref(Type)|ReturnTypes]) :-
	recolt_args_lists(RestArgs, ReturnTypes).

% Convertir une liste de paramètres et de types avec la modalité var en une liste de types transformés.
type_ref([], []).
type_ref([(varp(X),T) | Params], [(X,ref(T)) | TransformedParams]) :-
    type_ref(Params, TransformedParams).

type_ref([(X,T) | Params], [(X,T) | TransformedParams]) :-
    \+ X = var(_),
    type_ref(Params, TransformedParams).

% Type checking for a list of expressions
check_type(_,[],[]).
check_type(G,[X|Y],[T|Z]) :- type_expr(G,X,T), check_type(G,Y,Z).
% Type checking for a list of args of procedures
check_type_argsproc(_, [], []).
check_type_argsproc(G, [Arg|Args], [Type|ArgTypes]) :-
    type_expr_param(G, Arg, Type),
    check_type_argsproc(G, Args, ArgTypes).

% regles de typage
% expressionsar

% Expar pour une référence (passage par adresse)
type_expr_param(G, adr(X), ref(T)) :- 
    atom(X),
    member((X, ref(T)), G).

% Expar pour une valeur (passage par valeur)
type_expr_param(G, E, T) :- 
    type_expr(G, E, T).

% expressions

% num
type_expr(_, num(N), int) :- 
        integer(N).

% id
% idr - Identifiants pointant vers des références (ref T)
type_expr(G, id(X), T) :- 
    atom(X),
    member((X, ref(T)), G).

% idv - Identifiants pour des variables ou des constantes directes (autres que ref T)
type_expr(G, id(X), T) :- 
    atom(X),
    member((X, T), G).

% if
type_expr(G, if(Cond, Then, Else), Type) :-
    type_expr(G, Cond, bool),
    type_expr(G, Then, Type),
    type_expr(G, Else, Type).

% and
type_expr(G, and(Expr1, Expr2), bool) :-
    type_expr(G, Expr1, bool),
    type_expr(G, Expr2, bool).

% or
type_expr(G, or(Expr1, Expr2), bool) :-
    type_expr(G, Expr1, bool),
    type_expr(G, Expr2, bool).

% abs
type_expr(G, abs(ARGS, Expr), arrow(ArgTypes, ReturnType)) :-
    recolt_args_lists(ARGS, ArgTypes), 
    append(G, ARGS, ExtendedContext), 
    type_expr(ExtendedContext, Expr, ReturnType).  % Vérifie le type de l'expression dans le contexte étendu

% app
type_expr(G, app(Func, Args), Type) :-
    maplist(type_expr(G), Args, Types),
    type_expr(G, Func, arrow(Types, Type)).

% instructions

% echo
type_stat(G, echo(E), void) :-
    type_expr(G, E, int). 

%set
type_stat(G, set(Name, Expr), void) :-
    type_expr(G, id(Name), ref(Type)),
    type_expr(G,Expr, Type).

%IF
type_stat(G, if1(Expr, Block1, Block2), void) :-
    type_expr(G, Expr, bool),
    type_block(G, Block1, void),
    type_block(G, Block2, void).

%While
type_stat(G, while(Expr, Block), void) :-
    type_expr(G, Expr, bool),
    type_block(G, Block, void).

%Call 
type_stat(G, call(Name, Args), void) :-
    type_expr(G, id(Name), arrow(ArgTypes, void)),
    check_type_argsproc(G, Args, ArgTypes).
 
% definitions

% const
type_def(G, const(X, T, Expr), [(X, T)|G]) :-
    type_expr(G, Expr, T).

% fonctions

type_def(G, fun(FuncName, ARGS, Expr, Type), [(FuncName, arrow(ArgTypes,Type))|G]) :-
    recolt_args_lists(ARGS, ArgTypes), % Get the types of arguments
    append(G, ARGS, ExtendedContext), % Extend the context with the new function arguments
    type_expr(ExtendedContext, Expr, Type). % Verify the type of the expression in the extended context

type_def(G, funrec(FuncName, ARGS, Expr, Type), [(FuncName, arrow(ArgTypes, Type))|G]) :-
    recolt_args_lists(ARGS, ArgTypes),
    append([(FuncName, arrow(ArgTypes, Type))|G], ARGS, ExtendedContext),
    type_expr(ExtendedContext, Expr, Type).

%var
% Modification d'une définition de variable pour inclure son type comme 'ref(Type)'
type_def(G, var(X, Type), [(X, ref(Type))|G]) :-
    (Type = int ; Type = bool).

/*proc */
type_def(G, proc(ProcName, ARGS, Block), [(ProcName, arrow(TransformedParamTypes,void))|G]) :-
    type_ref(ARGS, TransformedParams),
    recolt_args_lists(TransformedParams, TransformedParamTypes),
    append(G, TransformedParams, ExtendedContext),
    type_block(ExtendedContext, Block, void).

type_def(G, procrec(ProcName, ARGS, Block), [(ProcName, arrow(TransformedParamTypes,void))|G]) :-
    type_ref(ARGS, TransformedParams),
    recolt_args_lists(TransformedParams, TransformedParamTypes),
    append(G, ARGS, ExtendedContext), 
    type_block([(ProcName, arrow(TransformedParamTypes,void))|ExtendedContext], Block, void). 

 % liste de commandes

 % defs

 %end
type_cmds(_, [], void).

type_cmds(G, [def(X)|Cmds], void) :-
    type_def(G, X, G1),
    type_cmds(G1, Cmds, void).

type_cmds(G, [Stat|Cmds], void) :- 
    type_stat(G, Stat, void),
    type_cmds(G, Cmds, void).

% block
type_block(G, block(Cmds), void) :-
    type_cmds(G,Cmds ,void).

% programme
type_prog(G,prog(X), void) :- type_block(G, X, void).

% main 
% affiche ok si programme bien typé, ko sinon

main :-
    gamma_0(Gamma),
    Cmds = [echo(const(add(mul(42,x),sub(div(y,2),42))))], % Exemple de programme
    type_cmds(Gamma, Cmds, void),
    write('ok'), nl, !.
main :-
    write('ko'), nl.

main_stdin :-
	gamma_0(Gamma),
	read(user_input,T),
	type_prog(Gamma,T,R),
    print(R).
% Test expressions

test_num:- type_expr(_, num(5), int) -> write('test_num passed'); write('test_num failed').

test_id:- gamma_0(Liste), type_expr(Liste, true, bool) -> write('test_id passed'); write('test_id failed').

test_if:-   
gamma_0(Liste),
    T = int,                   
    type_expr(Liste, if(true, num(42), num(0)), T) ->                
    write('test_if passed with type '), write(T);  write('test_if failed').
test_and :-    
    gamma_0(Liste),                   
    type_expr(Liste, and(true, "uio" ),bool)->  write('test_and passed');  write('test_and failed').

test_or :-    
    gamma_0(Liste),                        
    type_expr(Liste, or(true, baguette),bool) ->  write('test_or passed');  write('test_or failed').

test_app :-
gamma_0(Liste),                                 
    type_expr(Liste, app(add, [num(2), num(2)]), int) -> write('test_app passed') ;  write('test_app failed').

tests_expressions:-
    test_num,
    nl,  
    test_id,
    nl,
    test_if,
    nl,
    test_and,
    nl,
    test_or,
    nl,
    test_app.

%Test instructions

test_echo :-     
gamma_0(Liste),             
    type_stat(Liste, echo(num(42)),void) -> write('test_echo passed'); write('test_echo failed').

%Test instructions

test_const :- 

gamma_0(Liste),
     type_def(Liste, const(x, int, num(42)),G1),
     type_expr(G1, x, int) -> write('test_const passed'); write('test_const failed').

test_abs :-
    gamma_0(Gamma),
    (   type_expr(Gamma, abs([(x,int)], x), arrow([int], int))
    ->  write('test_abs passed'), nl
    ;   write('test_abs failed'), nl
    ).

% Test a function definition where the body is an if expression

test_fun :-
    gamma_0(Gamma),
    type_def(Gamma, fun(myfunc, [(x,int)], num(1), int), NewContext),
    (type_expr(NewContext, myfunc, arrow([int], int))
    ->  write('test_fun passed'), nl
    ;   write('test_fun failed'), nl
    ).

test_funrec :-
    gamma_0(Gamma),
    type_def(Gamma, funrec(myfuncrec, [(x, int)], if(app(eq, [x, num(0)]), num(1), app(mul, [x, app(myfuncrec, [app(sub, [x, num(1)])])])), int), NewContext),
    (type_expr(NewContext, app(myfuncrec, [num(5)]), int)
    ->  write('test_funrec passed'), nl
    ;   write('test_funrec failed'), nl).



