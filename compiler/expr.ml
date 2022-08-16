(*
  <prim1> ::=
          | Not
*)
type prim1 = Not

(* 
  <comp>  ::=
          | Less
          | Equal
*)
type comp = Less | Equal

(*
  <prim2> ::=
          | Plus
          | Minus
          | Times
          | DividedBy
          | And
          | Or
          | (Comp <comp>)
          | Get
*)
type prim2 = Plus | Minus | Times | DividedBy | And | Or | Comp of comp | Get

type id = string

(*
<types> ::= int
        | bool
        | any
*)
type types = Int | Bool | Any

(* 
  <expr> ::= var
         |  number
         |  bool
         |  tuple
         |  (set <expr> <expr> <expr>)
         |  (prim1 <prim1> <expr>)
         |  (prim2 <prim2> <expr> <expr>)
         |  (if <expr> <expr> <expr>)
         |  (let (<id> * <expr>) list <expr>)
         |  (app <id> <expr> list)
         |  (lambda (<id>) list <expr>)
         |  (Clambda <id> (<types>) list <types>)
*)
type 'a expr =
  | Var of id * 'a
  | Num of int64 * 'a
  | Bool of bool * 'a
  | Tuple of 'a expr list * 'a
  | Set of 'a expr * 'a expr * 'a expr * 'a
  | Prim1 of prim1 * 'a expr * 'a
  | Prim2 of prim2 * 'a expr * 'a expr * 'a
  | If of 'a expr * 'a expr * 'a expr * 'a
  | Let of (id * 'a expr * 'a) list * 'a expr * 'a
  | App of 'a expr * 'a expr list * 'a
  | Lambda of string list * 'a expr * 'a
  | CLambda of id * types list * types * 'a

open Fmt

let pp_prim1 fmt prim pp_expr e =
  match prim with Not -> pf fmt "(not %a)" pp_expr e

let pp_comp fmt comp pp_expr e1 e2 =
  match comp with
  | Less -> pf fmt "(< %a %a)" pp_expr e1 pp_expr e2
  | Equal -> pf fmt "(= %a %a)" pp_expr e1 pp_expr e2

let pp_prim2 fmt prim pp_expr e1 e2 =
  match prim with
  | Plus -> pf fmt "(+ %a %a)" pp_expr e1 pp_expr e2
  | Minus -> pf fmt "(- %a %a)" pp_expr e1 pp_expr e2
  | Times -> pf fmt "(* %a %a)" pp_expr e1 pp_expr e2
  | DividedBy -> pf fmt "(/ %a %a)" pp_expr e1 pp_expr e2
  | And -> pf fmt "(and %a %a)" pp_expr e1 pp_expr e2
  | Or -> pf fmt "(or %a %a)" pp_expr e1 pp_expr e2
  | Comp comp -> pp_comp fmt comp pp_expr e1 e2
  | Get -> pf fmt "(get %a %a)" pp_expr e1 pp_expr e2

let pp_string_list = Fmt.(list ~sep:(const pf ";@") string)

let pp_types fmt = function
  | Int -> pf fmt "int"
  | Bool -> pf fmt "bool"
  | Any -> pf fmt "any"

let pp_list_types = Fmt.(list ~sep:(const pf ";@") pp_types)

let rec pp_expr fmt =
  let pp_let fmt = function
    | id, expr, _ -> pf fmt "(%a %a)" string id pp_expr expr
  in
  let pp_list = Fmt.(list ~sep:(const pf ";@") pp_let) in
  let pp_list_args = Fmt.(list ~sep:(const pf ";@") pp_expr) in
  function
  | Var (s, _) -> string fmt s
  | Num (n, _) -> int64 fmt n
  | Bool (b, _) -> bool fmt b
  | Prim1 (prim, e, _) -> pp_prim1 fmt prim pp_expr e
  | Prim2 (prim, e1, e2, _) -> pp_prim2 fmt prim pp_expr e1 e2
  | If (cond, e1, e2, _) ->
      pf fmt "(if %a %a %a)" pp_expr cond pp_expr e1 pp_expr e2
  | Let (ids, b, _) -> pf fmt "(let %a %a)" pp_list ids pp_expr b
  | App (f, args, _) -> pf fmt "(%a %a)" pp_expr f pp_list_args args
  | Tuple (es, _) -> pf fmt "(tuple %a)" pp_list_args es
  | Set (tup, num, value, _) ->
      pf fmt "(set %a %a %a)" pp_expr tup pp_expr num pp_expr value
  | Lambda (args, body, _) ->
      pf fmt "(lambda %a %a)" pp_string_list args pp_expr body
  | CLambda (id, types, ret_type, _) ->
      pf fmt "(CLambda %a %a %a)" string id pp_list_types types pp_types
        ret_type
