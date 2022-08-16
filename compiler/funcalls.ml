open Asm
open Expr
open Envs

let const_true = -1L

let const_false = 9223372036854775807L

let registers = [ RDI; RSI; RDX; RCX; R8; R9 ]

(* Slices a list in the range i, k *)
let slice (l : 'a list) (i : int) (k : int) : 'a list =
  let rec drop (l : 'a list) (i : int) =
    match l with
    | [] -> []
    | _ :: tl -> ( match i with 0 -> l | _ -> drop tl (i - 1))
  in
  let rec take (l : 'a list) (k : int) =
    match l with
    | [] -> []
    | hd :: tl -> ( match k with 0 -> [] | _ -> hd :: take tl (k - 1))
  in
  take (drop l i) (k - i + 1)

let try_gc (need : int64) (args_actuales : (string * arg) list) :
    instruction list =
  (* with the intent of not calculating registers in use, all registers will be pushed and popped *)
  let sliced_register = slice registers 0 (List.length args_actuales) in
  let push_list =
    List.concat (List.rev_map (fun reg -> [ IPush (Reg reg) ]) sliced_register)
  in
  let pop_list =
    List.concat (List.map (fun reg -> [ IPop (Reg reg) ]) sliced_register)
  in
  push_list
  @ [
      IMov (Reg RDI, Reg R15);
      IMov (Reg RSI, Const need);
      IMov (Reg RDX, Reg RBP);
      IMov (Reg RCX, Reg RSP);
      ICall (Label "try_gc");
      IMov (Reg R15, Reg RAX);
    ]
  @ pop_list

(* Function that generates the instructions for the correct calling of a function in the x86-64 convention *)
let funcall (fun_name : string) (args : arg list)
    (args_actuales : (string * arg) list) : instruction list =
  let sliced_registers =
    slice registers 0
      (min (max (List.length args) (List.length args_actuales) - 1) 5)
  in
  let sliced_registers2 = slice registers 0 (min (List.length args - 1) 5) in
  let call = [ ICall (Label fun_name) ] in
  let push_list =
    List.concat (List.rev_map (fun reg -> [ IPush (Reg reg) ]) sliced_registers)
  in
  let mov_list =
    List.concat
      (List.map2
         (fun reg arg -> [ IMov (Reg reg, arg) ])
         (List.rev sliced_registers2)
         (List.rev (slice args 0 (min (List.length args - 1) 5))))
  in
  let pop_list =
    List.concat (List.map (fun reg -> [ IPop (Reg reg) ]) sliced_registers)
  in
  if List.length args <= 6 then push_list @ mov_list @ call @ pop_list
  else
    let extra_args = slice args 6 (List.length args) in
    let push_extra =
      List.concat
        (List.rev_map
           (fun arg -> [ IMov (Reg R11, arg); IPush (Reg R11) ])
           extra_args)
    in
    push_list @ push_extra @ mov_list @ call
    @ [
        IAdd
          (Reg RSP, Const (Int64.mul 8L (Int64.of_int (List.length extra_args))));
      ]
    @ pop_list

let funcall_closure (args : arg list) (args_actuales : (string * arg) list) :
    instruction list =
  let new_registers = slice registers 1 6 in
  let sliced_registers =
    slice new_registers 0
      (min (max (List.length args) (List.length args_actuales) - 1) 5)
  in
  let sliced_registers2 =
    slice new_registers 0 (min (List.length args - 1) 5)
  in
  let call =
    [ ISub (Reg RAX, Const 0x5L) ]
    @ [ IMov (Reg R11, RegOffset (RAX, 1L)) ]
    @ [ ICall (Reg R11) ]
  in
  let push_list =
    List.concat
      (List.rev_map
         (fun reg -> [ IPush (Reg reg) ])
         ([ RDI ] @ sliced_registers))
  in
  let mov_list =
    List.concat
      (List.map2
         (fun reg arg -> [ IMov (Reg reg, arg) ])
         (List.rev sliced_registers2)
         (List.rev (slice args 0 (min (List.length args - 1) 4))))
  in
  let pop_list =
    List.concat
      (List.map (fun reg -> [ IPop (Reg reg) ]) ([ RDI ] @ sliced_registers))
  in
  if List.length args <= 5 then
    push_list @ mov_list @ [ IMov (Reg RDI, Reg RAX) ] @ call @ pop_list
  else
    let extra_args = slice args 5 (List.length args) in
    let push_extra =
      List.concat
        (List.rev_map
           (fun arg -> [ IMov (Reg R11, arg); IPush (Reg R11) ])
           extra_args)
    in
    push_list @ push_extra @ mov_list
    @ [ IMov (Reg RDI, Reg RAX) ]
    @ call
    @ [
        IAdd
          (Reg RSP, Const (Int64.mul 8L (Int64.of_int (List.length extra_args))));
      ]
    @ pop_list

(* Functions that generates the correct instruccions for a callee to be called in the x86-64 convention *)
let funcallee (args : string list) (num_local : int64) :
    instruction list * instruction list * (string * arg) list =
  let offset :
      (string * arg) list * int64 -> string -> (string * arg) list * int64 =
   fun (offsets, count) arg ->
    ( offsets @ [ (arg, RegOffset (RBP, Int64.add 2L count)) ],
      Int64.add count 1L )
  in
  let new_registers = slice registers 1 6 in
  let sliced_registers = slice new_registers 0 (min (List.length args - 1) 5) in
  let first_args = slice args 0 (min (List.length args - 1) 4) in
  let last_args = slice args 5 (List.length args) in
  let offsets, _ = List.fold_left offset ([], 0L) last_args in
  let final_args =
    List.combine first_args (List.map (fun reg -> Reg reg) sliced_registers)
  in
  let i_start =
    [ IPush (Reg RBP) ]
    @ [ IMov (Reg RBP, Reg RSP) ]
    @ [ ISub (Reg RSP, Const (Int64.mul 8L num_local)) ]
  in
  let i_end = [ IMov (Reg RSP, Reg RBP) ] @ [ IPop (Reg RBP) ] @ [ IRet ] in
  (i_start, i_end, final_args @ offsets)

(* 8*i+2  -> generar una lista de estos pa combinarlos con lastargs
   asociar primero argumento con registro
   asociar luego argumento con su posicion en la pila, el regoffset
   RegOffset(RBP, Int64.mul (-1L) offset)
*)

(*luego expulsar el numero de argumentos*)

(* Function for transforming values from our own to values recognizable by C *)
let transform_to_C_value (arg_type : types) (arg_tag : int) (count : int) :
    instruction list =
  match arg_type with
  | Int -> [ ISar (Reg RAX, Const 1L) ]
  | Bool ->
      [ ICmp (Reg RAX, Const const_true) ]
      @ [ IMov (Reg RAX, Const 0L) ]
      @ [ IJne (Label (Printf.sprintf "transform_tag_%d_%d" arg_tag count)) ]
      @ [ IMov (Reg RAX, Const 1L) ]
      @ [ ILabel (Label (Printf.sprintf "transform_tag_%d_%d" arg_tag count)) ]
  | Any -> []

(* Function that transform values from C to our own values *)
let transform_from_C_value (ret_type : types) (ret_tag : int) (count : int) :
    instruction list =
  match ret_type with
  | Int -> [ Ilsl (Reg RAX, Const 1L) ]
  | Bool ->
      [ ICmp (Reg RAX, Const 1L) ]
      @ [ IMov (Reg RAX, Const const_false) ]
      @ [ IJne (Label (Printf.sprintf "transform_tag_%d_%d" ret_tag count)) ]
      @ [ IMov (Reg RAX, Const const_true) ]
      @ [ ILabel (Label (Printf.sprintf "transform_tag_%d_%d" ret_tag count)) ]
  | Any -> []

(* Function that generates the correct instructions for calling a C function and converting the value returned *)
(* let cfuncall (fun_name : string) (args : arg list)
    (args_actuales : (string * arg) list) (ret_type : types) : instruction list
    =
  let instructions = funcall fun_name args args_actuales in
  let transformation =
    transform_from_C_value ret_type (Printf.sprintf "from_%s" fun_name)
  in
  instructions @ transformation *)

let rec find_free (args : id list) (body : int expr) (acum : id list) : id list
    =
  let rec find_arg (args : id list) (id : id) (acum : id list) : id list =
    match args with
    | [] -> acum @ [ id ]
    | id' :: rest -> if String.equal id' id then acum else find_arg rest id acum
  in
  let rec find_in_acum (acum : id list) (id : id) : bool =
    match acum with
    | [] -> false
    | id' :: rest -> if String.equal id' id then true else find_in_acum rest id
  in
  let rec binding_ids (bindings : (id * int expr * int) list) (args : id list)
      (acum : id list) : id list * id list =
    match bindings with
    | [] -> (args, acum)
    | (id, e, _) :: rest ->
        binding_ids rest (args @ [ id ]) (find_free args e acum)
  in
  match body with
  | Var (id, _) -> if find_in_acum acum id then acum else find_arg args id acum
  | Tuple (le, _) ->
      List.fold_left (fun acum e -> acum @ find_free args e acum) acum le
  | Set (tup, i, value, _) ->
      let acum' = find_free args tup acum in
      let acum'' = find_free args i acum' in
      find_free args value acum''
  | Prim1 (_, e, _) -> find_free args e acum
  | Prim2 (_, e1, e2, _) ->
      let acum' = find_free args e1 acum in
      find_free args e2 acum'
  | If (cond, e1, e2, _) ->
      let acum' = find_free args cond acum in
      let acum'' = find_free args e1 acum' in
      find_free args e2 acum''
  | Let (bindings, e, _) ->
      let args', acum' = binding_ids bindings args acum in
      find_free args' e acum'
  (* aca hay que añadir a args las que son creadas por el let o no*)
  | App (f, l, _) ->
      let acum' = find_free args f acum in
      List.fold_left (fun acum e -> find_free args e acum) acum' l
  | Lambda (arg, body, _) ->
      let args' = args @ arg in
      (* y en estas añadir a args args*)
      find_free args' body acum
  | _ -> acum

let make_closure (tag : int) (args : id list) (offsets_and_regs : lambdaenv) :
    instruction list =
  let copy_into_closure ((instr, heap_offset) : instruction list * int64)
      ((_, offset, arg) : id * int64 * arg) : instruction list * int64 =
    if offset != -1L then
      ( instr
        @ [
            IMov (Reg RAX, RegOffset (RBP, Int64.neg offset));
            IMov (RegOffset (R15, heap_offset), Reg RAX);
          ],
        Int64.add 1L heap_offset )
    else
      ( instr
        @ [ IMov (Reg RAX, arg); IMov (RegOffset (R15, heap_offset), Reg RAX) ],
        Int64.add 1L heap_offset )
  in
  let arity = List.length args in
  let name = Printf.sprintf "lambda_tag_%d" tag in
  let n_free_vars = List.length offsets_and_regs in
  let instr_vars, _ =
    List.fold_left copy_into_closure ([], 3L) offsets_and_regs
  in
  (* aridad *)
  [ IMov (Reg RAX, Const (Int64.of_int arity)) ]
  @ [ IMov (RegOffset (R15, 0L), Reg RAX) ]
  (* etiqueta *)
  @ [ IMov (Reg RAX, Label name) ]
  @ [ IMov (RegOffset (R15, 1L), Reg RAX) ]
  (* n de variables libres *)
  @ [ IMov (Reg RAX, Const (Int64.of_int n_free_vars)) ]
  @ [ IMov (RegOffset (R15, 2L), Reg RAX) ]
  (* variables libres *)
  @ instr_vars
  (* create closure *)
  @ [ IMov (Reg RAX, Reg R15) ]
  (* tag *)
  @ [ IAdd (Reg RAX, Const 5L) ]
  @ [ IAdd (Reg R15, Const (Int64.mul 8L (Int64.of_int (n_free_vars + 3)))) ]
  @ if n_free_vars mod 2 == 0 then [ IAdd (Reg R15, Const 8L) ] else []

let make_C_closure (tag : int) (types : types list) : instruction list =
  let arity = List.length types in
  let name = Printf.sprintf "C_lambda_tag_%d" tag in
  (* aridad *)
  [ IMov (Reg RAX, Const (Int64.of_int arity)) ]
  @ [ IMov (RegOffset (R15, 0L), Reg RAX) ]
  (* etiqueta *)
  @ [ IMov (Reg RAX, Label name) ]
  @ [ IMov (RegOffset (R15, 1L), Reg RAX) ]
  @ [ IMov (Reg RAX, Reg R15) ]
  (* tag *)
  @ [ IAdd (Reg RAX, Const 5L) ]
  @ [ IAdd (Reg R15, Const 16L) ]

let unmake_closure (free_vars : id list) : instruction list * env =
  let copy_from_closure_and_make_env
      ((instr, heap_offset, env) : instruction list * int64 * env) (id : id) :
      instruction list * int64 * env =
    let env', offset = extend_env id env in
    ( instr
      @ [
          IMov (Reg RAX, RegOffset (R11, heap_offset));
          IMov (RegOffset (RBP, Int64.neg offset), Reg RAX);
        ],
      Int64.add 1L heap_offset,
      env' )
  in
  let instr_to_copy, _, env =
    List.fold_left copy_from_closure_and_make_env ([], 3L, []) free_vars
  in
  let n_free_vars = List.length free_vars in
  ( [
      (* IPush (Reg RBP);
         IMov (Reg RBP, Reg RSP); *)
      ISub (Reg RSP, Const (Int64.mul 8L (Int64.of_int n_free_vars)));
      IMov (Reg R11, Reg RDI);
      ISub (Reg R11, Const 5L);
    ]
    @ instr_to_copy,
    env )

(* (lambda (x) (let (y z) (+ y + (w x)))) *)
