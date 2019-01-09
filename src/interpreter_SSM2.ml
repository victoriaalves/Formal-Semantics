#use "utils.ml" ;;
#use "instruction_SSM2.ml" ;;

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
