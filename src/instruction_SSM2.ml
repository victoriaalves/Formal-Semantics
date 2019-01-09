type variable = string

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

exception SSM2_Compiler_Error 
exception SSM2_Interpreter_Error ;;
