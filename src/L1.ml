(* Libraries *)
#use "utils.ml" ;;
#use "term_L1.ml" ;;
#use "bigstep_L1.ml"
#use "typeinfer_L1.ml" ;;


let t1 = Num(3) ;;
let t2 = Bool(true) ;;
let t3 = Bop(Sum, t1, t1) ;;
let t4 = Bop(Equal, t1, t1) ;;
let t5 = Bop(Equal, t2, t2) ;;
let t6 = Bop(Equal, t2, t1) ;; (* deve dar erro *)
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
let t17 = Bop(NotEqual, t2, t1) ;; (* erro *)
let t18 = If(t2,t3,t15) ;; (*deve dar int*)
let t19 = If(t2,t4,t5) ;; (*deve dar bool*)
let t20 = If(t2,t3,t5) ;; (*deve dar erro*)
let env = [] ;;
let t21 = "variavelInt" ;;
let currentEnv = updateT (t21) (TyInt) (env);;
let t22 = Fun(t21,TyInt,t15);; (* int -> int *)
let t23 = Fun(t21,TyInt,t5);; (* int -> bool *)
let t24 = "variavelBool" ;;
let currentEnv2 = updateT (t24) (TyBool) (env) ;;
let t25 = App(t22,t1) ;; (* t22: int -> int   t1: int    e1e2: int *)
let t27 = App(t23,t14) ;; (* t23:  int -> bool   e2: int    e1e2: bool , *)
let t26 = App(t23,t7) ;; (* t23: int -> bool   e2: bool    e1e2: bool , erro *)

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


(* Testes Big Step *)


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

Printf.printf "Verificando big step - Let: Let(varInt, TyInt, Bop(Sum,Num(2),Num(2)), Bop(Sum,Num(1),Num(2))) -----> " ;;
value2string (eval [] letest2)  ;;
print_newline();;

Printf.printf "Verificando big step - LetRec: Lrec(varInt, (TyInt,TyInt), (varInt,TyInt,ifexpInt), Bop(Sum,Num(1),Num(2))) -----> " ;;
value2string (eval [] letrec)  ;;
print_newline();;
Printf.printf "Verificando big step - LetRec: Lrec(varBool, (TyInt,TyBool), (varInt,TyInt,ifexpInt), If( Bop(Equal,Bool(true),Bool(false) ), Bool(true), Bool(false)) ) -----> " ;;
value2string (eval [] letrec2)  ;;
print_newline();;

Printf.printf "Verificando big step - bop Sum: Bop(Sum,Num(1),Num(2)) -----> " ;;
value2string (expBopSum)  ;;

print_newline();;

Printf.printf "Verificando big step - bop Equal: Bop(Equal,Bool(true),Bool(false)) -----> " ;;
value2string (expBopEqual) ;;
print_newline();;

Printf.printf "Verificando big step - if: If( Bop(Equal,Bool(true),Bool(false)), Bool(true), Bool(false)) -----> " ;;
value2string (eval [] ifexp ) ;;
print_newline();;

Printf.printf "Verificando big step - if: If( Bop(Equal,Bool(true),Bool(false)), Bool(true), Num(3)) -----> " ;;
value2string (eval [] ifexpInt )  ;;
print_newline();;

Printf.printf "Verificando big step - fun: Fun( varInt, TyInt, Bop(Mult,Num(5),Num(4))) -----> " ;;
value2string (eval [] funTest ) ;;
print_newline();;
Printf.printf "Verificando big step - fun: Fun( varBool, TyBool, Bop(Equal,Num(5),Num(4))) -----> " ;;
value2string (eval [] funTest2 )  ;;
print_newline();;

Printf.printf "Verificando big step - var: Var(varBool) -----> " ;;
value2string (eval valueEnv2 varTest ) ;;
print_newline();;

Printf.printf "Verificando big step - app: App( Fun( varInt, TyInt, Bop(Mult,Num(5),Num(4))), Num(4)) -----> ";;
value2string (eval [] app )  ;;
Printf.printf "\n\n\n";;

