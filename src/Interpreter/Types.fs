module Interpreter.Types

type id = string
type expr =
    | Var of id
    | Lam of id list * expr
    | App of expr * expr        
    | Bool of bool
    | Int of int
    | Cond of expr * expr * expr
    | Float of float
    | List of expr list
    | Let of id * expr * expr
    | LetRec of id * expr * expr
    | PrimitiveOperation of id
    | Op of id * int * expr list
    | Closure of expr * env
    | RClosure of expr * env * id
    | Prog of expr list
    | Print of expr
    | None
and env = Map<id, expr>
