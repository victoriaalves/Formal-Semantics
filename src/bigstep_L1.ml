#use "term_L1.ml" ;;
#use "utils.ml" ;;

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
