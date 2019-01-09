#use "instruction_SSM2.ml" ;;
#use "term_L1.ml" ;;

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
