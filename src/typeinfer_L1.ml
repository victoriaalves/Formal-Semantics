#use "term_L1.ml" ;;
#use "utils.ml" ;;

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
