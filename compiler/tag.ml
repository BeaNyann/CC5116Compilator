open Expr

(* Tags an expression with ascending values *)

let tag_expr (e : int expr) : int expr =
  let rec help (e : int expr) (cur : int) : int expr * int =
    (* help_list tags the values inside a list *)
    let rec help_list (ids : (id * int expr * int) list)
        (cur_list : (id * int expr * int) list) (cur : int) :
        (id * int expr * int) list * int =
      match ids with
      | [] -> (cur_list, cur + 1)
      | (id, e, _) :: rest ->
          let tag_e, next_tag = help e (cur + 1) in
          help_list rest (cur_list @ [ (id, tag_e, next_tag) ]) (next_tag + 1)
    in
    let rec help_args (args : int expr list) (cur_list : int expr list)
        (cur : int) : int expr list * int =
      match args with
      | [] -> (cur_list, cur + 1)
      | e :: rest ->
          let tag_e, next_tag = help e (cur + 1) in
          help_args rest (cur_list @ [ tag_e ]) next_tag
    in
    match e with
    | Var (s, _) -> (Var (s, cur), cur + 1)
    | Num (n, _) -> (Num (n, cur), cur + 1)
    | Bool (b, _) -> (Bool (b, cur), cur + 1)
    | Tuple (l, _) ->
        let tag_exprs, next_tag = help_args l [] (cur + 1) in
        (Tuple (tag_exprs, cur), next_tag)
    | Set (tup, i, num, _) ->
        let tag_tup, next_tag_0 = help tup (cur + 1) in
        let tag_i, next_tag_1 = help i next_tag_0 in
        let tag_num, next_tag_2 = help num next_tag_1 in
        (Set (tag_tup, tag_i, tag_num, cur), next_tag_2)
    | Prim1 (prim1, e, _) ->
        let tag_e, next_tag = help e (cur + 1) in
        (Prim1 (prim1, tag_e, cur), next_tag)
    | Prim2 (prim2, e1, e2, _) ->
        let tag_e1, next_tag = help e1 (cur + 1) in
        let tag_e2, next_next_tag = help e2 next_tag in
        (Prim2 (prim2, tag_e1, tag_e2, cur), next_next_tag)
    | If (cond, e1, e2, _) ->
        let tag_cond, next_tag = help cond (cur + 1) in
        let tag_e1, next_next_tag = help e1 next_tag in
        let tag_e2, next_next_next_tag = help e2 next_next_tag in
        (If (tag_cond, tag_e1, tag_e2, cur), next_next_next_tag)
    | Let (ids, e, _) ->
        let tag_ids, next_tag = help_list ids [] (cur + 1) in
        let tag_e, next_next_tag = help e next_tag in
        (Let (tag_ids, tag_e, cur), next_next_tag)
    | App (f, args, _) ->
        let tagged_f, next_tag = help f (cur + 1) in
        let tag_args, next_tag' = help_args args [] next_tag in
        (App (tagged_f, tag_args, cur), next_tag')
    | Lambda (args, body, _) ->
        let tagged_body, next_tag = help body (cur + 1) in
        (Lambda (args, tagged_body, cur), next_tag)
    | CLambda (id, types, ret_type, _) ->
        (CLambda (id, types, ret_type, cur), cur + 1)
  in
  let tagged, _ = help e 0 in
  tagged
