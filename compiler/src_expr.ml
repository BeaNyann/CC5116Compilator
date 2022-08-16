open Expr

(* 
  <src_expr> ::= var
         |  number
         |  bool
         |  tuple
         |  (set <src_expr> <src_expr> <src_expr>)
         |  (prim1 <prim1> <src_expr>)
         |  (prim2 <prim2> <src_expr> <src_expr>)
         |  (if <src_expr> <src_expr> <src_expr>)
         |  (let (<id> * <src_expr>) list <src_expr>)
         |  (app <id> <src_expr> list)
         |  (lambda (<id>) list <src_expr>)
         |  (Clambda <id> (<types>) list <types>)
*)

type src_expr =
  | Var of id
  | Num of int64
  | Bool of bool
  | Tuple of src_expr list
  | Set of src_expr * src_expr * src_expr
  | Destruct of id list * src_expr
  | Prim1 of prim1 * src_expr
  | Prim2 of prim2 * src_expr * src_expr
  | If of src_expr * src_expr * src_expr
  | Let of (id * src_expr) list * src_expr
  | Seq of src_expr * src_expr
  | App of src_expr * src_expr list
  | Lambda of string list * src_expr
  | CLambda of id * types list * types

(*
 <src_decl> ::= (Dfun <id> <id> list <src_expr>)
        | (DCfun <id> <types> list <types>) 
*)
type src_decl =
  | DFun of string * string list * src_expr
  | DCFun of string * types list * types
  | DRecord of string * string list

(* 
  <program> ::= (Program <src_decl> list <expr>)
*)
type src_program = Program of src_decl list * src_expr

open Fmt

let pp_string_list = Fmt.(list ~sep:(const pf ";@") string)

let rec pp_src_expr fmt =
  let pp_let fmt = function
    | id, expr -> pf fmt "(%a %a)" string id pp_src_expr expr
  in
  let pp_list = Fmt.(list ~sep:(const pf ";@") pp_let) in
  let pp_list_args = Fmt.(list ~sep:(const pf ";@") pp_src_expr) in
  function
  | Var s -> string fmt s
  | Num n -> int64 fmt n
  | Bool b -> bool fmt b
  | Prim1 (prim, e) -> pp_prim1 fmt prim pp_src_expr e
  | Prim2 (prim, e1, e2) -> pp_prim2 fmt prim pp_src_expr e1 e2
  | If (cond, e1, e2) ->
      pf fmt "(if %a %a %a)" pp_src_expr cond pp_src_expr e1 pp_src_expr e2
  | Let (ids, b) -> pf fmt "(let %a %a)" pp_list ids pp_src_expr b
  | App (f, args) -> pf fmt "(%a %a)" pp_src_expr f pp_list_args args
  | Tuple es -> pf fmt "(tuple %a)" pp_list_args es
  | Seq (e1, e2) -> pf fmt "(seq %a %a)" pp_src_expr e1 pp_src_expr e2
  | Set (tup, num, value) ->
      pf fmt "(set %a %a %a)" pp_src_expr tup pp_src_expr num pp_src_expr value
  | Destruct (ids, e) ->
      pf fmt "(destruct %a %a)" pp_string_list ids pp_src_expr e
  | Lambda (args, body) ->
      pf fmt "(lambda %a %a)" pp_string_list args pp_src_expr body
  | CLambda (name, types, ret_type) ->
      pf fmt "(Clambda %a (%a) -> %a" string name pp_list_types types pp_types
        ret_type

let pp_src_program fmt =
  let pp_decl fmt = function
    | DFun (id, args, body) ->
        pf fmt "(def %a (%a) %a)" string id pp_string_list args pp_src_expr body
    | DCFun (id, arg_types, ret_type) ->
        pf fmt "(defsys %a %a -> %a)" string id pp_list_types arg_types pp_types
          ret_type
    | DRecord (id, fields) ->
        pf fmt "(record %a %a)" string id pp_string_list fields
  in
  let pp_decl_list = Fmt.(list ~sep:(const pf ";@") pp_decl) in
  function
  | Program (decls, body) ->
      pf fmt "(%a %a)" pp_decl_list decls pp_src_expr body
