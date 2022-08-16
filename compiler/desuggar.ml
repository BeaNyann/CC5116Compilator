open Expr
open Src_expr
open Envs

(* Desuggars a source expression to a core expression, transforming records into tuples and destructing the tuples when posibble *)
let rec desuggar_expr (renv : recordenv) (expr : src_expr) : int expr =
  (* Checks a let declaration for destruct instructions *)
  let rec desuggar_let (binds : (string * src_expr) list)
      (acum : (string * int expr * int) list) : (string * int expr * int) list =
    (* Destructs a binding *)
    let rec desuggar_destruct (l : string list) (t : int expr) (c : int64)
        (acum : (string * int expr * int) list) : (string * int expr * int) list
        =
      match l with
      | id :: tl ->
          desuggar_destruct tl t (Int64.add 1L c)
            (acum @ [ (id, Prim2 (Get, t, Num (c, 1), 1), 1) ])
      | [] -> acum
    in
    match binds with
    | ("", Destruct (l, e)) :: rest ->
        desuggar_let rest
          (acum @ desuggar_destruct l (desuggar_expr renv e) 0L [])
    | (id, e) :: rest ->
        desuggar_let rest (acum @ [ (id, desuggar_expr renv e, 1) ])
    | [] -> acum
  in
  (* Desuggars a record into a tuple declaration and the corresponding getters *)
  let desuggar_record (id : string list) (l : src_expr list) (renv : recordenv)
      : int expr =
    let rec find_field (field : string) (fields : string list) (count : int64) :
        int64 =
      match fields with
      | id :: tl ->
          if field = id then count else find_field field tl (Int64.add 1L count)
      | [] -> Fmt.failwith "Field not found: %s" field
    in
    let _, fields = lookuprecordenv (List.hd id) renv in
    let field = List.hd (List.tl id) in
    let count = 0L in
    Prim2
      ( Get,
        desuggar_expr renv (List.hd l),
        Num (find_field field fields count, 1),
        1 )
  in
  match expr with
  | Var s -> Var (s, 1)
  | Num n -> Num (n, 1)
  | Bool b -> Bool (b, 1)
  | Tuple l -> Tuple (List.map (fun e -> desuggar_expr renv e) l, 1)
  | Seq (e1, e2) ->
      Let ([ ("DONT_CARE", desuggar_expr renv e1, 1) ], desuggar_expr renv e2, 1)
  | Set (tup, i, n) ->
      Set (desuggar_expr renv tup, desuggar_expr renv i, desuggar_expr renv n, 1)
  | Prim1 (prim, e) -> Prim1 (prim, desuggar_expr renv e, 1)
  | Prim2 (prim, e1, e2) ->
      Prim2 (prim, desuggar_expr renv e1, desuggar_expr renv e2, 1)
  | If (cond, e1, e2) ->
      If
        ( desuggar_expr renv cond,
          desuggar_expr renv e1,
          desuggar_expr renv e2,
          1 )
  | Let (binds, e) -> Let (desuggar_let binds [], desuggar_expr renv e, 1)
  | App (f, l) -> (
      match f with
      | Var id ->
          let id' = Str.split_delim (Str.regexp "-") id in
          (* (record point x y z) *)
          if List.length id' == 2 && List.length l == 1 then
            (* point 1 2 3 *)
            desuggar_record id' l renv (* point-x (point 1 2 3)*)
          else
            let exists, fields = lookuprecordenv (List.hd id') renv in
            if exists == false then
              App
                ( desuggar_expr renv f,
                  List.map (fun e -> desuggar_expr renv e) l,
                  1 )
            else if List.compare_length_with l (List.length fields) == 0 then
              Tuple (List.map (fun e -> desuggar_expr renv e) l, 1)
            else Fmt.failwith "arity is not correct for record %s" id
      | _ ->
          App
            (desuggar_expr renv f, List.map (fun e -> desuggar_expr renv e) l, 1)
      )
  | Destruct (_, _) -> Fmt.failwith "Not valid"
  | CLambda (id, types, ret_type) -> CLambda (id, types, ret_type, 1)
  | Lambda (args, body) -> Lambda (args, desuggar_expr renv body, 1)

let rec desuggar_decl (d : src_decl list) (lets : (id * src_expr) list)
    (renv : recordenv) (externs : string list) :
    (id * src_expr) list * recordenv * string list =
  match d with
  | [] -> (lets, renv, externs)
  | decl :: rest -> (
      match decl with
      (* Funs and CFuns are stored as lets that define a lambda or CLamda, records are stored for desuggaring later *)
      | DFun (id, args, e) ->
          desuggar_decl rest (lets @ [ (id, Lambda (args, e)) ]) renv externs
      | DCFun (id, types, ret_type) ->
          desuggar_decl rest
            (lets @ [ (id, CLambda (id, types, ret_type)) ])
            renv
            (externs @ [ Printf.sprintf "extern %s" id ])
      | DRecord (id, fields) ->
          desuggar_decl rest lets (extend_recordenv id fields renv) externs)

let rec make_lets (lets : src_expr list) (e : src_expr) : src_expr =
  match lets with
  | [] -> e
  | lets' :: rest -> (
      match lets' with
      | Let (ids, _) -> Let (ids, make_lets rest e)
      | _ -> Fmt.failwith "Not declared properly")

let desuggar (program : src_program) : int expr * string =
  match program with
  | Program (decls, e) ->
      let lets, renv, externs = desuggar_decl decls [] [] [] in
      let final_body = Let (lets, e) in
      (desuggar_expr renv final_body, String.concat "\n" externs)
