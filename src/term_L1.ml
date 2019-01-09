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
