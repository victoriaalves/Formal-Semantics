#use "term_L1.ml" ;;
#use "instruction_SSM2.ml" ;;

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
