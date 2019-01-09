(*
	Federal University of Rio Grande do Sul
	Institute of Informatics
	INF005516 - Formal Semantics N
	Final project:
		Consists of the implementation of an interpreter composed of an expression evaluator, implemented following the
		rules of operational semantics given in the big step style, and an inference of types.
		Both must follow strictly the operational semantics and the type system defined for L1. The expression evaluator
		should be implemented following the rules of operational semantics given in the big step style.
		Extra implementations: An interpreter for the abstract machine language SSM2 and the compilation of L1 for the
		abstract machine language SSM2.
	Professor: Alvaro Freitas Moreira
	Group members: Lisiane Aguiar
             	   Marcely Zanon Boito
             	   Victoria Elizabetha Alves
*)

(* ** Terms ** *)
type variable = string

type operator = Sum | Diff | Mult | Div | Equal | NotEqual | GreaterOrEqual | And | Or | Less | LessOrEqual | Greater

type tipo  = TyInt | TyBool | TyFn of tipo * tipo

type expr = Num of int
          | Bool of bool
          | Bop of operator * expr * expr
          | If of expr * expr * expr
          | Var of variable
          | App of expr * expr
          | Fun of variable * tipo * expr
          | Let of variable * tipo * expr * expr
          | Lrec of variable * (tipo * tipo) * (variable * tipo * expr) * expr

type value = Vnum of int
           | Vbool of bool
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and
     env = (variable * value) list
and
     envt = (variable * tipo) list
and
     envExp = (variable * expr) list

exception Eval_Error of string
;;


(* ** Utils ** *)

exception Variable_not_found ;;

(* Size function for lists *)
let rec size x = match x with
     [] -> 0
   | _::tail_x -> 1 + (size tail_x) ;;


(* Function to transform term in string *)
let rec type2string t = match t with
    TyInt -> "int"
    | TyBool -> "bool"
    | TyFn(t1,t2) -> let t1' = type2string(t1) in let t2' = type2string(t2) in "fun("^t1'^"->"^t2'^")" ;;

(* Function to transform value in string *)
let rec value2string v : unit = match v with
	| Vnum(n) -> Printf.printf "Vnum(%d)" n
	| Vbool(b) ->Printf.printf "Vbool(%b)" b
	| Vclos (var,exp,env) -> Printf.printf "Vclos (var,exp,env)"
	| Vrclos (var,ver,exp,env) -> Printf.printf "Vrclos (var, ver, exp, env)" ;;


(* Function to update environment *)
let update variable v environment : env = match environment with
  | [] -> [(variable, v)]
  | hd::tl -> List.append [(variable, v)] environment ;;


(* Function to look up for variable in environment *)
let rec lookup variable environment : value = match environment with
  | [] -> raise Variable_not_found
  | (name, v)::tl ->
    if (name == variable)      (* Found the variable in the head *)
    then v                     (* Returns variable value *)
    else lookup variable tl ;; (* Look for it in the tale *)



(* Function to update environment of TYPES *)
let updateT variable tipo environment : envt = match environment with
  | [] -> [(variable, tipo)]
  | hd::tl -> List.append [(variable,tipo)] environment ;;



(* Function to look up for variable in environment of TYPES *)
let rec lookupT variable environment : tipo = match environment with
  | [] -> raise Variable_not_found
  | (name, v)::tl ->
    if (name == variable)        (* Found the variable in the head *)
    then v                      (* Returns variable value *)
    else lookupT variable tl ;; (* Look for it in the tale *)

(* ** Type Infer ** *)
exception InvalidType ;;

let rec typecheck environment exp : tipo = match exp with
  (* Values *)
    Num(v)  -> TyInt
  | Bool(b) -> TyBool

  (* Binary operations *)
  | Bop(op,exp1,exp2) ->
    let ope1 = typecheck environment exp1 in
    let ope2 = typecheck environment exp2 in
    (match op, ope1, ope2 with
          Sum, TyInt, TyInt -> TyInt
        | Diff, TyInt, TyInt -> TyInt
        | Mult, TyInt, TyInt -> TyInt
        | Div, TyInt, TyInt -> TyInt
        | Equal, TyBool, TyBool -> TyBool
        | Equal, TyInt, TyInt -> TyBool
        | NotEqual, TyBool, TyBool -> TyBool
        | NotEqual, TyInt, TyInt -> TyBool
        | GreaterOrEqual, TyInt, TyInt -> TyBool
        | And, TyBool, TyBool -> TyBool
        | Or, TyBool, TyBool -> TyBool
        | Less, TyInt, TyInt -> TyBool
        | LessOrEqual, TyInt, TyInt -> TyBool
        | Greater, TyInt, TyInt -> TyBool
        | _ -> raise InvalidType
      )

  (* Conditional *)
  | If(e1,e2,e3) ->
      let tipoe1 = typecheck environment e1 in
      let tipoe2 = typecheck environment e2 in
      let tipoe3 = typecheck environment e3 in
          if tipoe1 == TyBool && tipoe2 == tipoe3 then tipoe2 else raise InvalidType

   (* Function *)
  | Fun(v, t, e) -> TyFn(t, (typecheck(updateT v t environment) e))

  (* Variable *)
  | Var(v) -> lookupT v environment

 (* Application *)
  | App(e1,e2) ->
    let exp1 = typecheck environment e1 in
    let exp2 = typecheck environment e2 in
    (match exp1 with
      TyFn(t1,t2) -> (if t1 == exp2
          then t2
          else raise InvalidType)
      |_ -> raise InvalidType)

  (* Let *)
  | Let(variable, t, e1, e2) ->
    let x = typecheck environment e1 in
    (if x == t (*if the expression e1 is right (of type t)*)
        then typecheck (updateT variable t environment) e2 (*ten the output is of typecheck(e2)*)
        else raise InvalidType)

  (* Let Rec *)
  | Lrec(func, (ty1, ty2), (var, ty3, e1), e2) ->
    let ty4 = TyFn(ty1,ty2) in
    let update1 = updateT var ty3 environment in
    let update2 = updateT func ty4 update1 in
    let ty5 = typecheck update2 e1 in (
      if ty5 == ty2 && ty1 == ty3
      then typecheck (updateT func ty4 environment) e2
      else raise InvalidType
    )
;;

(* ** Big step ** *)
exception InvalidEval ;;

let rec eval environment e : value = match e with

  (* Values (BS-NUM e BS-BOOL) *)
    Num(v)  -> Vnum(v)
  | Bool(b) -> Vbool(b)

  (* Binary operations  (BS-OP)*)
  | Bop(op,exp1,exp2) ->
    let ope1 = eval environment exp1 in
    let ope2 = eval environment exp2 in
    (match op, ope1, ope2 with
          Sum, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 + exp2)
        | Diff, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 - exp2)
        | Mult, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 * exp2)
        | Div, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 / exp2)
        | Equal, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 = exp2)
        | Equal, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 = exp2)
        | NotEqual, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 <> exp2)
        | NotEqual, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 <> exp2)
        | GreaterOrEqual, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 >= exp2)
        | And, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 && exp2)
        | Or, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 || exp2)
        | Less, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 < exp2)
        | LessOrEqual, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 <= exp2)
        | Greater, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 > exp2)
	| _ -> raise InvalidEval
      )


  (*  Conditional (BS-IF)*)
  | If(e1, e2, e3) when ((eval environment e1) = Vbool(true)) -> eval environment e2
  | If(e1, e2, e3) when ((eval environment e1) = Vbool(false)) -> eval environment e3


  (* Function *)
  | Fun(v, t, e) -> Vclos(v, e, environment)


  (* Variable (BS-ID)*)
  | Var(v) -> lookup v environment


  (* Application *)
  | App(e1, e2) ->
      let exp1 = eval environment e1 in
      let exp2 = eval environment e2 in
      (match exp1, exp2 with
        Vclos(variable, e, envA), value -> eval (update variable value envA) e
        | Vrclos(f, x, e, envA), value -> eval (update f (Vrclos(f, x, e, envA)) (update x value envA)) e
        | _ -> raise InvalidEval
      )

    (* Let (BS-LET)*)
    | Let(variable, t, e1, e2) ->
        let exp1 = eval environment e1 in (*evaluate e1*)
        let env2 = (update variable exp1 environment) in (*creates {x->v} + env*)
        eval env2 e2 (*evaluates e2 in this new environment*)

    (* Let Rec *)
    | Lrec(f, (t1, t2), (variable, t3, e1), e2) -> (*{let rec f = fn x => e1 in e2}*)
        let exp1 = eval environment (Fun(variable, t3, e1)) in
        (*let recenv = update f exp1 environment in (*{f -> {f,variable,e1,env}}*)
        let evale2 = eval recenv e2 in
        let exp1 = eval environment (Fun(variable, t3, e1)) in*)
        (match exp1 with
          | Vclos(x, e, env) -> eval (update f (Vrclos(f, x, e, environment)) env) e2
          | _ -> raise InvalidEval
        )

	| _ -> raise InvalidEval
;;

(******************** Tests Type Infer ** *)

let t1 = Num(3) ;;
let t2 = Bool(true) ;;
let t3 = Bop(Sum, t1, t1) ;;
let t4 = Bop(Equal, t1, t1) ;;
let t5 = Bop(Equal, t2, t2) ;;
let t6 = Bop(Equal, t2, t1) ;; 
let t7 = Bop(NotEqual, t1, t1) ;;
let t8 = Bop(GreaterOrEqual, t1, t1) ;;
let t9 = Bop(And, t2, t2) ;;
let t10 = Bop(Or, t2, t2) ;;
let t11 = Bop(Less, t1, t1) ;;
let t12 = Bop(LessOrEqual, t1, t1) ;;
let t13 = Bop(Greater, t1, t1) ;;
let t14 = Bop(Diff, t1, t1) ;;
let t15 = Bop(Mult, t1, t1) ;;
let t16 = Bop(Div, t1, t1) ;;
let t17 = Bop(NotEqual, t2, t1) ;; 
let t18 = If(t2,t3,t15) ;; 
let t19 = If(t2,t4,t5) ;; 
let t20 = If(t2,t3,t5) ;; 
let env = [] ;;
let t21 = "variavelInt" ;;
let currentEnv = updateT (t21) (TyInt) (env);;
let t22 = Fun(t21,TyInt,t15);; (* int -> int *)
let t23 = Fun(t21,TyInt,t5);; (* int -> bool *)
let t24 = "variavelBool" ;;
let currentEnv2 = updateT (t24) (TyBool) (env) ;;
let t25 = App(t22,t1) ;; 
let t27 = App(t23,t14) ;; 
let t26 = App(t23,t7) ;; 

(* e1: T  vaar: T  e2: T', let é T' *)
let t28 = Let(t24,TyBool, t19, t16) ;;
let t29 = Let(t21,TyInt, t16, t19) ;;
let t30 = Let(t21,TyInt, t19, t16) ;; (* deve dar erro *)
let t31 = Lrec(t21,(TyInt,TyInt),(t21,TyInt,t15),t16);;
let t32 = Lrec(t24,(TyBool,TyBool),(t24,TyBool,t5),t12);;
let t33 = Lrec(t24,(TyBool,TyInt),(t24,TyBool,t5),t12);; (* deve dar erro *)

(* Testes para let rec - ok *)
Printf.printf "Verificando tipo let rec: Lrec(variavelInt, (TyInt,TyInt),(variavelInt,TyInt, Bop(Mult, Num(3) , Num(3) ) ), Bop(Div, Num(3), Num(3)) )   -----> %s" (type2string (typecheck (currentEnv) (t31))) ;;

print_newline();;
print_newline();;

Printf.printf "Verificando tipo let rec: Lrec(variavelBool, (TyBool,TyBool), (variavelBool,TyBool, Bop(Equal, Bool(true), Bool(true)) ), Bop(LessOrEqual, Num(3), Num(3)) )   -----> %s" (type2string (typecheck (currentEnv2) (t32))) ;;

print_newline();;
print_newline();;

(* Testes para let *)
Printf.printf "Verificando tipo let: Let(variavelBool, TyBool, If(Bool(true), Bop(Equal, Num(3), Num(3)), Bop(Equal, Bool(true), Bool(true)) ), Bop(Div, Num(3), Num(3)) )    -----> %s" (type2string (typecheck (currentEnv2) (t28))) ;; 

print_newline();;
print_newline();;

Printf.printf "Verificando tipo let: Let(variavelInt,TyInt, Bop(Div, Num(3), Num(3)), If(Bool(true), Bop(Equal, Num(3), Num(3)),Bop(Equal, Bool(true), Bool(true))))        -----> %s" (type2string (typecheck (currentEnv) (t29))) ;; 

print_newline();;
print_newline();;

(* Testes para app - ok *)
Printf.printf "Verificando tipo app: App( Fun(variavelInt, TyInt, Bop(Mult, Num(3), Num(3)) ), Num(3))  -----> %s" (type2string (typecheck (currentEnv) (t25))) ;; 

print_newline();;
print_newline();;

Printf.printf "Verificando tipo app: App( Fun(variavelInt,TyInt, Bop(Equal,  Bool(true),  Bool(true)) ) , Bop(Diff, Num(3), Num(3)))        -----> %s" (type2string (typecheck (currentEnv) (t27))) ;; 

print_newline();;
print_newline();; 


(* Testes para tipo Num, Bool, Bop - Ok *)
Printf.printf "Verificando tipo num: Num(3)        -----> %s" (type2string (typecheck [] t1)) ;;

print_newline();;
print_newline();;

Printf.printf "Verificando tipo bool: Bool(true)        -----> %s" (type2string (typecheck [] t2)) ;;

print_newline();;
print_newline();;

Printf.printf "Verificando tipo op Sum: Bop(Sum, Num(3), Num(3))        -----> %s" (type2string (typecheck [] t3)) ;;
print_newline();;
print_newline();;

Printf.printf "Verificando tipo op Equal: Bop(Equal, Num(3), Num(3))        -----> %s" (type2string (typecheck [] t4)) ;;
print_newline();;
print_newline();;

Printf.printf "Verificando tipo op Equal: Bop(Equal, Bool(true), Bool(true))       -----> %s" (type2string (typecheck [] t5)) ;;
print_newline();;


(* Testes para o if - ok *)
print_newline();;
Printf.printf "Verificando if: If( Bool(true) , Bop(Sum, Num(3), Num(3)), Bop(Mult, Num(3), Num(3)) )  -----> %s" (type2string (typecheck [] t18)) ;;

print_newline();;
print_newline();;

(* Testes para o fun - ok *)
Printf.printf "Verificando fun: Fun(variavelInt, TyInt, Bop(Mult, Num(3), Num(3)) )  -----> %s" (type2string (typecheck currentEnv t22)) ;; 
print_newline();;
print_newline();;
Printf.printf "Verificando fun: Fun(variavelInt, TyInt,  Bop(Equal, Bool(true), Bool(true)) )   -----> %s" (type2string (typecheck currentEnv t23)) ;;
print_newline();;
print_newline();;
(* Testes para var - ok *)

Printf.printf "Verificando var: variavelInt  -----> %s" (type2string (typecheck (currentEnv) (Var(t21))) ) ;;
print_newline();;
print_newline();;
Printf.printf "Verificando var: variavelBool  -----> %s" (type2string (typecheck (currentEnv2) (Var(t24))) ) ;;
print_newline();;
print_newline();;

(***************** Testes que devem falhar *)

(*

Printf.printf "Verificando tipo let rec: Lrec(variavelBool, (TyBool,TyInt), (variavelBool,TyBool, Bop(Equal, Bool(true), Bool(true))   -----> %s" (type2string (typecheck (currentEnv2) (t33))) ;;

print_newline();;

Printf.printf "Verificando tipo op Equal: Bop(Equal, Bool(true), Num(3)) - Teste não deve passar  -----> %s" (type2string (typecheck [] t6)) ;;

print_newline();;

Printf.printf "Verificando if: If( Bool(true) , Bop(Equal, Num(3), Num(3)) , Bop(Equal, Bool(true), Bool(true)))  -----> %s" (type2string (typecheck [] t19)) ;;

print_newline();;

Printf.printf "Verificando if: If( Bool(true) , Bop(Sum, Num(3), Num(3)), Bop(Equal, Bool(true), Bool(true)) )  -----> %s" (type2string (typecheck [] t20)) ;;

print_newline();;

Printf.printf "Verificando tipo app: App( Fun(variavelInt,TyInt, Bop(Equal,  Bool(true),  Bool(true)) ), Bop(NotEqual, Num(3), Num(3)) )        -----> %s" (type2string (typecheck (currentEnv) (t26))) ;; 

print_newline();;

Printf.printf "Verificando tipo let: Let( variavelInt,TyInt, If(Bool(true), Bop(Equal, Num(3), Num(3) ), Bop (Equal, Bool(true), Bool(true) ) ), Bop(Div, Num(3), Num(3)))        -----> %s" (type2string (typecheck (currentEnv) (t30))) ;; 

print_newline();;

*)


(**************** Testes Big Step *)


(*let evalList = t1:: (t2:: (t3 :: [])) ;;
let output = List.map (fun x -> (typecheck [] x)) evalList ;;
let strings = List.map (fun t -> (type2string t)) output ;;*)

(* CONJUNTO DE TESTES *)
let numTesting = Vnum(1);;
let boolTesting = Vbool(true);;

let varInt = "varTest";;
let valueEnv =  update varInt numTesting [];;
let varBool = "varTestBool";;
let valueEnv2 =  update varBool boolTesting [];;

let vclosTesting = Vclos(varInt,t22,valueEnv);;
let vclosTesting = Vclos(varBool,t22,valueEnv2);;
let vclosTesting = Vrclos(varBool,varBool,t4,valueEnv2);;

let expBopE = Bop(Equal,Bool(true),Bool(false));;
let expBopE2 = Bop(Equal,Bool(false),Bool(true));;
let expBopSum = eval [] (Bop(Sum,Num(1),Num(2))) ;;
let expBopEqual= eval [] (Bop(Equal,Bool(true),Bool(false))) ;;

let ifexp = If(expBopE,Bool(true),Bool(false)) ;;
let ifexpInt = If(expBopE,Bool(true),Num(3)) ;;

let funTest = Fun(varInt,TyInt,Bop(Mult,Num(5),Num(4)));;
let funTest2 = Fun(varBool,TyBool,Bop(Equal,Num(5),Num(4)));;

let varTest = Var(varBool) ;;

let app = App(funTest,Num(4));; (* ela nao funcionou com o bop equal *)

let letest = Let(varInt,TyInt,Bop(Sum,Num(2),Num(2)),Bop(Sum,Num(1),Num(2))) ;;
let letest2 = Let(varBool,TyInt,Bop(Sum,Num(2),Num(2)),Bop(Equal,Bool(true),Bool(false))) ;;

let letrec = Lrec(varInt, (TyInt,TyInt), (varInt,TyInt,ifexpInt), Bop(Sum,Num(1),Num(2))) ;;
let letrec2 = Lrec(varBool, (TyInt,TyBool), (varInt,TyInt,ifexpInt), ifexp) ;;

Printf.printf "Verificando big step - Let: Let(varInt, TyInt, Bop(Sum,Num(2),Num(2)), Bop(Sum,Num(1),Num(2))) -----> " ;;
value2string (eval [] letest)  ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - Let: Let(varInt, TyInt, Bop(Sum,Num(2),Num(2)), Bop(Sum,Num(1),Num(2))) -----> " ;;
value2string (eval [] letest2)  ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - LetRec: Lrec(varInt, (TyInt,TyInt), (varInt,TyInt,ifexpInt), Bop(Sum,Num(1),Num(2))) -----> " ;;
value2string (eval [] letrec)  ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - LetRec: Lrec(varBool, (TyInt,TyBool), (varInt,TyInt,ifexpInt), If( Bop(Equal,Bool(true),Bool(false) ), Bool(true), Bool(false)) ) -----> " ;;
value2string (eval [] letrec2)  ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - bop Sum: Bop(Sum,Num(1),Num(2)) -----> " ;;
value2string (expBopSum)  ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - bop Equal: Bop(Equal,Bool(true),Bool(false)) -----> " ;;
value2string (expBopEqual) ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - if: If( Bop(Equal,Bool(true),Bool(false)), Bool(true), Bool(false)) -----> " ;;
value2string (eval [] ifexp ) ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - if: If( Bop(Equal,Bool(true),Bool(false)), Bool(true), Num(3)) -----> " ;;
value2string (eval [] ifexpInt )  ;;

print_newline();;
print_newline();;

Printf.printf "Verificando big step - fun: Fun( varInt, TyInt, Bop(Mult,Num(5),Num(4))) -----> " ;;
value2string (eval [] funTest ) ;;
print_newline();;
print_newline();;

Printf.printf "Verificando big step - fun: Fun( varBool, TyBool, Bop(Equal,Num(5),Num(4))) -----> " ;;
value2string (eval [] funTest2 )  ;;
print_newline();;
print_newline();;

Printf.printf "Verificando big step - var: Var(varBool) -----> " ;;
value2string (eval valueEnv2 varTest ) ;;
print_newline();;
print_newline();;

Printf.printf "Verificando big step - app: App( Fun( varInt, TyInt, Bop(Mult,Num(5),Num(4))), Num(4)) -----> ";;
value2string (eval [] app )  ;;
Printf.printf "\n\n\n";;

Printf.printf "\n\n\n";;
(* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *)
(* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *)
(* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *)
(* ** SSM2 STRUCTURE ** *)

type instruction = INT of int
                 | BOOL of bool
                 | POP
                 | COPY
                 | ADD
                 | INV
                 | PROD
                 | DIV
                 | EQ
                 | GTE
                 | GT
                 | AND
                 | OR
                 | NOT
                 | JUMP of int
                 | JUMPIFTRUE of int
                 | VAR of variable
                 | FUN of variable * code (*FUN(x,c)*)
                 | RFUN of variable * variable * code (*RFUN(f,x,c)*)
                 | APPLY
              and
                 code = instruction list

type storableValue = SVINT of int
                  | SVBOOL of bool
                  | SVCLOS of ssm2_env * variable * code
                  | SVRCLOS of ssm2_env * variable * variable * code
                  and
                    ssm2_env = (variable * storableValue) list
                  and
                    stack = storableValue list
                  and
                    dump = (code * stack * ssm2_env) list

type  state = STATE of code * stack * ssm2_env * dump

exception SSM2_Compiler_Error ;;
exception SSM2_Interpreter_Error ;;

(* ** SSM2 UTILS ** *)

exception Split_Error ;;

let rec split_n number s_list =
  if number == 0 then ([], s_list) else( match s_list with
                                    [] -> raise Split_Error
                                    | head::tail ->
                                      let (l1, l2) = split_n (number-1) tail in (head::l1,l2))
                                      ;;

let rec updateSSM2 variable value environment : ssm2_env = match environment with
    [] -> [(variable, value)]
    |head::tail -> List.append [(variable, value)] environment ;;

let rec lookupSSM2 variable environment : storableValue = match environment with
    [] -> raise Variable_not_found
    | (name, value)::tail ->
      if(name == variable) then value else lookupSSM2 variable tail ;;

(* ** SSM2 Compiler ** *)

let rec ssm2_compiler environment expr : code = match expr with
    (* Values *)
      Num(v)  -> [INT(v)]
    | Bool(b) -> [BOOL(b)]

    (*Binary operations*)
    | Bop(op,exp1,exp2) ->
      let comp1 = ssm2_compiler environment exp1 in
      let comp2 = ssm2_compiler environment exp2 in
      (match op with
        Sum -> List.append comp2 (List.append comp1 [ADD])
      | Diff -> List.append comp2 (List.append (List.append [INV] comp1) [ADD]) (*subtraction = sum with one of them inv*)
      | Mult -> List.append comp2 (List.append comp1 [PROD])
      | Div -> List.append comp2 (List.append comp1 [DIV])
      | Equal -> List.append comp2 (List.append comp1 [EQ])
      | NotEqual -> List.append comp2 (List.append comp1 (List.append [EQ] [NOT]))
      | And -> List.append comp2 (List.append comp1 [AND])
      | Or -> List.append comp2 (List.append comp1 [OR])
      | Less -> let inv_comp2 = List.append [INV] comp2 in
                let inv_comp1 = List.append [INV] comp1 in
                List.append inv_comp2 (List.append inv_comp1 [GT])
      | LessOrEqual -> let inv_comp2 = List.append [INV] comp2 in
                       let inv_comp1 = List.append [INV] comp1 in
                       List.append inv_comp2 (List.append inv_comp1 [GTE]) (* x < y == -x > -y*)
      | Greater -> List.append comp2 (List.append comp1 [GT])
      | GreaterOrEqual -> List.append comp2 (List.append comp1 [GTE])
      (*| _ -> raise SSM2_Compiler_Error*)
      )

    (* Conditional *)
    | If(e1, e2, e3) -> (*if = comp_e1 + (jumpiftrue (n3+1)) + compe3 + (jump n2) + compye2 *)
      let comp_e1 = ssm2_compiler environment e1 in
      let comp_e2 = ssm2_compiler environment e2 in
      let comp_e3 = ssm2_compiler environment e3 in
      let length_e2 = size comp_e2 in
      let length_e3 = (size comp_e3) + 1 in
      let c_if = List.append [JUMPIFTRUE length_e3] comp_e3 in
      let c_else = List.append [JUMP length_e2] comp_e2 in
      List.append (List.append comp_e1 c_if) c_else

    (* Function *)
    | Fun(v, t, e) -> [FUN(v, (ssm2_compiler environment e))]

    (* Variable *)
    | Var(v) -> [VAR(v)]

    (* Application *)
    | App(e1, e2) ->
      let comp_e1 = ssm2_compiler environment e1 in
      let comp_e2 = ssm2_compiler environment e2 in
      List.append comp_e2 (List.append comp_e1 [APPLY])

    (* Let *)
    | Let(variable, t, e1, e2) ->
      let comp_e1 = ssm2_compiler environment e1 in
      let comp_e2 = ssm2_compiler environment e2 in
      List.append comp_e1 (List.append [FUN(variable, comp_e2)] [APPLY])

    (* Let Rec *)
    | Lrec(f, (t1, t2), (variable, t3, e1), e2) ->
      let comp_e1 = ssm2_compiler environment e1 in
      let comp_e2 = ssm2_compiler environment e2 in
      List.append [RFUN(f, variable, comp_e1)] (List.append [FUN(f, comp_e2)] [APPLY])

    (*| _ -> raise SSM2_Compiler_Error*)
    ;;

(* ** SSM2 Interpreter ** *)

let rec ssm2_interpreter code stack environment dump : state = match code with
  (*restore return point*)
  | [] -> (match dump with
          [] -> STATE(code, stack, environment, dump)
        | (c, s, e)::dump_tl -> ssm2_interpreter c (List.append stack s) e dump_tl)

  (*Basic types*)
  | INT(n)::code_tl -> ssm2_interpreter code_tl (List.append [SVINT(n)] stack) environment dump
  | BOOL(n)::code_tl -> ssm2_interpreter code_tl (List.append [SVBOOL(n)] stack) environment dump

  (*POP: remove stack top *)
  | POP::code_tl -> (match stack with
                    [] -> raise SSM2_Interpreter_Error (*pop on an empty stack*)
                    | hd::stack_tl -> ssm2_interpreter code_tl stack_tl environment dump )

  (*COPY*)
  | COPY::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error (*copy on an empty stack*)
                      | hd::stack_tl -> ssm2_interpreter code_tl (List.append [hd] stack) environment dump)

  (*ADD*)
  | ADD::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error (*add on an empty stack*)
                      | SVINT(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error (*we need at least two elements for add*)
                                            | SVINT(z2)::stack_tl_tl ->
                                              let sum_z = z1 + z2 in
                                              ssm2_interpreter code_tl (List.append [SVINT(sum_z)] stack_tl_tl) environment dump
                                            | _ -> raise SSM2_Interpreter_Error )
                  | _ -> raise SSM2_Interpreter_Error)
  (*INV*)
  | INV::code_tl -> (match stack with
                    [] -> raise SSM2_Interpreter_Error
                    | SVINT(z1)::stack_tl ->
                      let inv_z = z1 * (-1) in
                      ssm2_interpreter code_tl (List.append [SVINT(inv_z)] stack_tl) environment dump
                    | _ -> raise SSM2_Interpreter_Error)

  (*PROD*)
  | PROD::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVINT(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVINT(z2)::stack_tl_tl ->
                                                let mul_z = z1 * z2 in
                                                ssm2_interpreter code_tl (List.append [SVINT(mul_z)] stack_tl_tl) environment dump
                                              | _ -> raise SSM2_Compiler_Error)
                      | _ -> raise SSM2_Interpreter_Error)
  (*DIV*)
  | DIV::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVINT(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVINT(z2)::stack_tl_tl ->
                                                if z2 == 0 then (raise SSM2_Interpreter_Error) else (
                                                let div_z = z1 / z2 in
                                                ssm2_interpreter code_tl (List.append [SVINT(div_z)] stack_tl_tl) environment dump)
                                              | _ -> raise SSM2_Compiler_Error)
                      | _ -> raise SSM2_Interpreter_Error)
  (*EQ*)
  | EQ::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVINT(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVINT(z2)::stack_tl_tl ->
                                                let eq_z = z1 == z2 in
                                                ssm2_interpreter code_tl (List.append [SVBOOL(eq_z)] stack_tl_tl) environment dump
                                              | _ -> raise SSM2_Compiler_Error)
                      |SVBOOL(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVBOOL(z2)::stack_tl_tl ->
                                                let eq_z = z1 == z2 in
                                                ssm2_interpreter code_tl (List.append [SVBOOL(eq_z)] stack_tl_tl) environment dump
                                              | _ -> raise SSM2_Compiler_Error)
                      | _ -> raise SSM2_Interpreter_Error)
  (*GTE*)
  | GTE::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVINT(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVINT(z2)::stack_tl_tl ->
                                                let gte_z = z1 >= z2 in
                                                ssm2_interpreter code_tl (List.append [SVBOOL(gte_z)] stack_tl_tl) environment dump
                                              | _ -> raise SSM2_Compiler_Error)
                      | _ -> raise SSM2_Interpreter_Error)
  (*GT*)
  | GT::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVINT(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVINT(z2)::stack_tl_tl ->
                                                let gt_z = z1 >= z2 in
                                                ssm2_interpreter code_tl (List.append [SVBOOL(gt_z)] stack_tl_tl) environment dump
                                              | _ -> raise SSM2_Compiler_Error)
                      | _ -> raise SSM2_Interpreter_Error)
  (*AND*)
  | AND::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVBOOL(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVBOOL(z2)::stack_tl_tl ->
                                                let and_z = z1 && z2 in
                                                ssm2_interpreter code_tl (List.append [SVBOOL(and_z)] stack_tl_tl) environment dump
                                              | _ -> raise SSM2_Compiler_Error)
                      | _ -> raise SSM2_Interpreter_Error)
  (*OR*)
  | OR::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVBOOL(z1)::stack_tl -> (match stack_tl with
                                              [] -> raise SSM2_Interpreter_Error
                                              | SVBOOL(z2)::stack_tl_tl ->
                                                let and_z = z1 || z2 in
                                                ssm2_interpreter code_tl (List.append [SVBOOL(and_z)] stack_tl_tl) environment dump
                                              | _ -> raise SSM2_Compiler_Error)
                      | _ -> raise SSM2_Interpreter_Error)
  (*NOT*)
  | NOT::code_tl -> (match stack with
                      [] -> raise SSM2_Interpreter_Error
                      |SVBOOL(z1)::stack_tl -> let not_z = not(z1) in
                                                ssm2_interpreter code_tl (List.append [SVBOOL(not_z)] stack_tl) environment dump
                      | _ -> raise SSM2_Interpreter_Error)
  (*JUMP*)
  |JUMP(n)::code_tl ->
    let(l1,new_code) = split_n n code in (*creates the new code jumping n instrucitons*)
    ssm2_interpreter new_code stack environment dump

  (*JUMPIFTRUE*)
  | JUMPIFTRUE(n)::code_tl -> (match stack with
      [] -> raise SSM2_Interpreter_Error
      | SVBOOL(z1)::stack_tl ->
        let (l1, new_code) = split_n n code in
        if z1 then (ssm2_interpreter new_code stack_tl environment dump)
        else (ssm2_interpreter code_tl stack_tl environment dump) (*either way the stack top has to pop*)
      | _ -> raise SSM2_Interpreter_Error (*stack top is not a bool*)
      )

  (*VAR*) (*finds the variable inside the environment*)
  | VAR(n)::code_tl -> ssm2_interpreter code_tl (List.append [(lookupSSM2 n environment)] stack) environment dump

  (*FUN*)
  | FUN(var, f_code)::code_tl -> ssm2_interpreter code_tl (List.append [SVCLOS(environment, var, f_code)] stack) environment dump

  (*RFUN*)
  | RFUN(f, var, f_code)::code_tl -> ssm2_interpreter code_tl (List.append [SVRCLOS(environment, f, var, f_code)] stack) environment dump

  (*APPLY*)
  | APPLY::code_tl -> (match stack with (*(APPLY:c,CLOS(e1,x,c')::sv::s,e,d) > (c',[],(x,sv)::e',(c,s,e)::d)*)
                      [] -> raise SSM2_Interpreter_Error
                      | SVCLOS(env, var, c)::stack_tl ->
                        (match stack_tl with
                            [] -> raise SSM2_Interpreter_Error (*we need a value for apply*)
                            | sv::stack_tl_tl -> ssm2_interpreter c [] (updateSSM2 var sv environment) (List.append [(code_tl, stack_tl_tl, environment)] dump)
                            )
(*(APPLY::c, RCLOS(e1,f,x,c')::sv::s,e,d) > (c',[], (f, RCLOS(e1,f,x,c'))::(x,sv)::e',(c,s,e)::d)*)
                      | SVRCLOS(env, f, var, c)::stack_tl ->
                        (match stack_tl with
                          [] -> raise SSM2_Interpreter_Error
                          | sv::stack_tl_tl -> ssm2_interpreter c [] (updateSSM2 f (SVRCLOS(env,f,var,c)) (updateSSM2 var sv env)) (List.append [(code_tl, stack_tl_tl, environment)] dump)
                          )
                      | _ -> raise SSM2_Interpreter_Error
                      )
;;


(* ** SSM2 TESTS ** *)

(* Não consigo fazer funcionar =(((((((((
let rec print_ssm2 v : code =
  let s = size v in
  let eq = s == 1 in
  match eq with
  | true -> Printf.printf List.hd v
  | false -> Printf.printf v[0] ; print_ssm2 List.tl v
  ;;
*)


let ssm2_int = ssm2_compiler [] (Num 5) ;;
let ssm2_bool = ssm2_compiler [] (Bool true) ;;

Printf.printf "Verificando o compilador de SSM2:\n" ;;
Printf.printf "%d\n" (size ssm2_int) ;;
(*print_ssm2 ssm2_int ;;*)
