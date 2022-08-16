open Expr
open Asm
open Tag
open Errors
open Funcalls
open Prims
open Envs
open Src_expr
open Desuggar

(* Constants definition *)

let min_int = Int64.div Int64.min_int 2L

let max_int = Int64.div Int64.max_int 2L

(* This functions calculates the necesary slots of the stack for the program to use in it's local variables,
usually considers the expression with the most slots required *)
let rec count_let (expr : int expr) (n : int64) : int64 =
  match expr with
  | Prim1 (_, e, _) -> Int64.add (count_let e 0L) n
  | Prim2 (_, e1, e2, _) ->
      Int64.add (max (count_let e1 2L) (count_let e2 2L)) n
  | If (cond, e1, e2, _) ->
      Int64.add
        (max (max (count_let e1 0L) (count_let e2 0L)) (count_let cond 0L))
        n
  | Set (tup, i, num, _) ->
      Int64.add
        (max (max (count_let tup 3L) (count_let i 3L)) (count_let num 3L))
        n
  | Tuple (le, _) ->
      let length_tuple = Int64.of_int (List.length le) in
      Int64.add
        (max length_tuple
           (List.fold_left max 0L (List.map (fun e -> count_let e 0L) le)))
        n
  | Let (b, e, _) ->
      let length_b = Int64.of_int (List.length b) in
      Int64.add
        (max (count_let e length_b)
           (List.fold_left max length_b
              (List.map (fun (_, e, _) -> count_let e length_b) b)))
        n
  | App (f, l, _) ->
      Int64.add (count_let f 0L)
        (Int64.add n
           (List.fold_left max
              (Int64.of_int (List.length l))
              (List.map (fun e -> count_let e (Int64.of_int (List.length l))) l)))
  | _ -> n

(* Compiles the full expression *)
let rec compile_expr (e : 'a expr) (env : env) (fargs : (string * arg) list) :
    instruction list =
  let build_bindings (fargs : (string * arg) list)
      ((instr, env) : instruction list * env) ((id, e, _) : id * 'a expr * 'a) :
      instruction list * env =
    let env', slot = extend_env id env in
    ( instr
      @ [ IComment (Label (Printf.sprintf "binding_%s" id)) ]
      @ compile_expr e env fargs
      @ [ IMov (RegOffset (RBP, Int64.neg slot), Reg RAX) ],
      env' )
  in
  let build_args (fargs : (string * arg) list)
      ((instr, args, count, arg_env) : instruction list * arg list * int * env)
      (expr : 'a expr) : instruction list * arg list * int * env =
    let env', slot = extend_env (Printf.sprintf "arg_%d" count) arg_env in
    let intr' =
      instr
      @ [ IComment (Label (Printf.sprintf "arg_%d" count)) ]
      @ compile_expr expr arg_env fargs
      @ [ IMov (RegOffset (RBP, Int64.neg slot), Reg RAX) ]
    in
    let arg' = args @ [ RegOffset (RBP, Int64.neg slot) ] in
    (intr', arg', count + 1, env')
  in
  match e with
  | Num (n, _) ->
      if n > max_int || n < min_int then
        failwith ("Integer overflow " ^ Int64.to_string n)
      else
        [ IComment (Label (Printf.sprintf "number_%d" (Int64.to_int n))) ]
        @ [ IMov (Reg RAX, Const n) ]
        @ [ Ilsl (Reg RAX, Const 1L) ]
  | Bool (true, _) ->
      [ IComment (Label (Printf.sprintf "true")) ]
      @ [ IMov (Reg RAX, Const const_true) ]
  | Bool (false, _) ->
      [ IComment (Label (Printf.sprintf "false")) ]
      @ [ IMov (Reg RAX, Const const_false) ]
  | Var (s, _) ->
      (* if there aare no fargs (outside of a function) we just check the enviroment, otherwise, we check the fargs first *)
      if fargs == [] then
        let offset = lookup false s env in
        [ IComment (Label (Printf.sprintf "variable_%s" s)) ]
        @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offset)) ]
      else
        let offset = lookup true s env in
        if Int64.compare (-1L) offset != 0 then
          [ IComment (Label (Printf.sprintf "variable_%s" s)) ]
          @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offset)) ]
        else
          let reg = List.assoc s fargs in
          [ IComment (Label (Printf.sprintf "variable_from_function_%s" s)) ]
          @ [ IMov (Reg RAX, reg) ]
  | Tuple (le, tag) ->
      let build_tuple (tag : int)
          ((args, count) : (string * 'a expr * int) list * int) (expr : 'a expr)
          : (string * 'a expr * int) list * int =
        let new_list = args @ [ (Printf.sprintf "%d" count, expr, tag) ] in
        (new_list, count + 1)
      in
      let bind_tuple (env : env) (instr : instruction list)
          ((id, _, _) : string * 'a expr * int) : instruction list =
        let slot = lookup false id env in
        let c = int_of_string id in
        instr
        @ [
            IComment
              (Label
                 (Printf.sprintf "tuple_slot_%s_value_%s" (Int64.to_string slot)
                    id));
          ]
        @ [ IMov (Reg R11, RegOffset (RBP, Int64.neg slot)) ]
        @ [ IMov (RegOffset (R15, Int64.of_int c), Reg R11) ]
      in
      (* List of expressions of the tuple values *)
      let expr_list, _ = List.fold_left (build_tuple tag) ([], 1) le in
      (* instructions to compile the tuple expresion *)
      let instr, env' =
        List.fold_left (build_bindings fargs) ([], env) expr_list
      in
      (* instructions to compile the tuple proper *)
      let instr_tuple = List.fold_left (bind_tuple env') [] expr_list in
      try_gc (Int64.of_int (List.length le + 1)) fargs
      @ instr
      @ [ IComment (Label (Printf.sprintf "tuple_tag_%d" tag)) ]
      @ [ IMov (Reg R11, Const (Int64.of_int (List.length le))) ]
      @ [ IMov (RegOffset (R15, 0L), Reg R11) ]
      @ instr_tuple
      @ [ IMov (Reg RAX, Reg R15) ]
      @ [ IAdd (Reg RAX, Const 1L) ]
      @ [ IComment (Label (Printf.sprintf "tuple_alignment_tag_%d" tag)) ]
      @ [
          IAdd
            (Reg R15, Const (Int64.mul 8L (Int64.of_int (List.length le + 1))));
        ]
      @ if List.length le mod 2 == 0 then [ IAdd (Reg R15, Const 8L) ] else []
  | Set (tup, i, value, tag) ->
      (* we create a false enviroment to store the relevant values*)
      let instr, env' =
        List.fold_left (build_bindings fargs) ([], env)
          [ ("tup", tup, tag); ("i", i, tag); ("value", value, tag) ]
      in
      let slot_tup = lookup false "tup" env' in
      let slot_i = lookup false "i" env' in
      let slot_value = lookup false "value" env' in
      (* Check that the i value is inside the tuple, and all other values match, then do the swap of the values *)
      instr
      @ [ IComment (Label (Printf.sprintf "set_tag_%d" tag)) ]
      @ check_number (RegOffset (RBP, Int64.mul (-1L) slot_i))
      @ [ IMov (Reg R11, RegOffset (RBP, Int64.mul (-1L) slot_i)) ]
      @ [ ISar (Reg R11, Const 1L) ]
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) slot_tup)) ]
      @ [ IMov (Reg RDX, RegOffset (RBP, Int64.mul (-1L) slot_value)) ]
      @ check_pair_rax
      @ [ ISub (Reg RAX, Const 1L) ]
      @ [ ICmp (Reg R11, Const 0L) ]
      @ [ IJl (Label "error_index_too_low") ]
      @ [ ICmp (Reg R11, RegOffset (RAX, 0L)) ]
      @ [ IJge (Label "error_index_too_high") ]
      @ [ IAdd (Reg R11, Const 1L) ]
      @ [ IImul (Reg R11, Const 8L) ]
      @ [ IAdd (Reg R11, Reg RAX) ]
      @ [ IMov (RegOffset (R11, 0L), Reg RDX) ]
  | Prim1 (prim, e, tag) -> compile_expr e env fargs @ compile_prim1 prim tag
  | Prim2 (prim, e1, e2, tag) ->
      let instr, env' =
        List.fold_left (build_bindings fargs) ([], env)
          [ ("l", e1, tag); ("r", e2, tag) ]
      in
      instr @ compile_prim2 prim env' tag
  | If (cond, e1, e2, tag) ->
      let else_label = Printf.sprintf "if_false_tag_%d" tag in
      let done_label = Printf.sprintf "done_tag_%d" tag in
      [ IComment (Label (Printf.sprintf "If_tag_%d" tag)) ]
      @ compile_expr cond env fargs
      @ check_boolean_rax
      @ [ ICmp (Reg RAX, Const const_true); IJne (Label else_label) ]
      @ compile_expr e1 env fargs
      @ [ IJmp (Label done_label); ILabel (Label else_label) ]
      @ compile_expr e2 env fargs
      @ [ ILabel (Label done_label) ]
  | Let (bindings, e, tag) ->
      let instr, env' =
        List.fold_left (build_bindings fargs) ([], env) bindings
      in
      [ IComment (Label (Printf.sprintf "Let_tag_%d" tag)) ]
      @ instr @ compile_expr e env' fargs
  | App (f, l, _) ->
      let instr, args, _, env' =
        List.fold_left (build_args fargs) ([], [], 0, env) l
      in
      instr @ compile_expr f env' fargs @ check_closure
      @ check_arity_rax (Int64.of_int (List.length l))
      @ funcall_closure args fargs
  | Lambda (args, body, tag) ->
      (* Check the number of free variables and create an special enviroment to store them *)
      let free_vars = find_free args body [] in
      let offsets_and_regs =
        List.fold_left
          (fun lambdaenv s ->
            let offset = lookup true s env in
            if Int64.compare (-1L) offset != 0 then
              extend_lambdaenv s offset (Reg R11) lambdaenv
            else
              let reg = List.assoc s fargs in
              extend_lambdaenv s (-1L) reg lambdaenv)
          [] free_vars
      in
      (* instructions for packing and unpacking the closure *)
      let closure_instr = make_closure tag args offsets_and_regs in
      let closure_unpack_instr, lambda_env = unmake_closure free_vars in
      let i_start, i_end, fargs' = funcallee args (count_let body 0L) in
      let jmplabel = Printf.sprintf "lambda_end_tag_%d" tag in
      [ IJmp (Label jmplabel) ]
      @ [ ILabel (Label (Printf.sprintf "lambda_tag_%d" tag)) ]
      @ i_start @ closure_unpack_instr
      @ compile_expr body lambda_env fargs'
      @ i_end
      @ [ ILabel (Label jmplabel) ]
      @ try_gc (Int64.of_int (List.length free_vars + 3)) fargs
      @ closure_instr
  | CLambda (id, types, ret_type, tag) ->
      (* make the C closure and check the registers, since they have to be shifted by one register to call the C function *)
      let closure_instr = make_C_closure tag types in
      let used_registers = slice registers 1 (min (List.length types) 5) in
      let dest_registers = slice registers 0 (min (List.length types - 1) 4) in
      (* Shiftingof the registers, also transforming them when necessary *)
      let transform_registers, _ =
        List.fold_left2
          (fun (instr, count) (orig_reg, dest_reg) t ->
            ( instr
              @ [ IComment (Label "transformacion") ]
              @ (match t with
                | Int -> check_number (Reg orig_reg)
                | Bool -> check_boolean (Reg orig_reg)
                | Any -> [])
              @ [ IMov (Reg RAX, Reg orig_reg) ]
              @ transform_to_C_value t tag count
              @ [ IPush (Reg dest_reg); IMov (Reg dest_reg, Reg RAX) ],
              count + 1 ))
          ([], 0)
          (List.combine used_registers dest_registers)
          (slice types 0 4)
      in
      (* pop the used registers *)
      let pop_list =
        List.concat
          (List.map (fun reg -> [ IPop (Reg reg) ]) (List.rev dest_registers))
      in
      let call = [ ICall (Label id) ] in
      let i_start = [ IPush (Reg RBP) ] @ [ IMov (Reg RBP, Reg RSP) ] in
      let i_end = [ IMov (Reg RSP, Reg RBP) ] @ [ IPop (Reg RBP) ] @ [ IRet ] in
      let jmplabel = Printf.sprintf "C_lambda_end_tag_%d" tag in
      if List.length types <= 5 then
        [ IJmp (Label jmplabel) ]
        @ [ ILabel (Label (Printf.sprintf "C_lambda_tag_%d" tag)) ]
        @ i_start @ transform_registers @ call
        @ transform_from_C_value ret_type tag 5
        @ pop_list @ i_end
        @ [ ILabel (Label jmplabel) ]
        @ closure_instr
      else
        (* if there are 6 registers used, since we have the self, the last one is in the stack and needs to be put into a register *)
        let transform_registers_6 =
          transform_registers
          @ [ IComment (Label "transformacion 6") ]
          @ (match List.nth types 5 with
            | Int -> check_number (RegOffset (RBP, 2L))
            | Bool -> check_boolean (RegOffset (RBP, 2L))
            | Any -> [])
          @ [ IMov (Reg RAX, RegOffset (RBP, 2L)) ]
          @ transform_to_C_value (List.nth types 5) tag 6
          @ [ IPush (Reg R9); IMov (Reg R9, Reg RAX) ]
        in
        let pop_list = [ IPop (Reg R9) ] @ pop_list in
        if List.length types <= 6 then
          [ IJmp (Label jmplabel) ]
          @ [ ILabel (Label (Printf.sprintf "C_lambda_tag_%d" tag)) ]
          @ i_start @ transform_registers_6 @ call
          @ transform_from_C_value ret_type tag 6
          @ pop_list @ i_end
          @ [ ILabel (Label jmplabel) ]
          @ closure_instr
        else
          (* if there are more registers, then we need to put them in the stack *)
          let extra_types = slice types 6 (List.length types) in
          let extra_transforms, _, _ =
            List.fold_left
              (fun (instr, count, count2) t ->
                ( instr
                  @ [ IComment (Label "transformacion extra") ]
                  @ (match t with
                    | Int -> check_number (RegOffset (RBP, count))
                    | Bool -> check_boolean (RegOffset (RBP, count))
                    | Any -> [])
                  @ [ IMov (Reg RAX, RegOffset (RBP, count)) ]
                  @ transform_to_C_value t tag count2
                  @ [ IPush (Reg RAX) ],
                  Int64.add count 1L,
                  count2 + 1 ))
              ([], 3L, 0) (List.rev extra_types)
          in
          [ IJmp (Label jmplabel) ]
          @ [ ILabel (Label (Printf.sprintf "C_lambda_tag_%d" tag)) ]
          @ i_start @ transform_registers_6 @ extra_transforms @ call
          @ transform_from_C_value ret_type tag (List.length types + 1)
          @ [
              IAdd
                ( Reg RSP,
                  Const (Int64.mul 8L (Int64.of_int (List.length extra_types)))
                );
            ]
          @ pop_list @ i_end
          @ [ ILabel (Label jmplabel) ]
          @ closure_instr

let compile_prog : src_program Fmt.t =
 fun fmt e ->
  let desuggared, externs = desuggar e in
  let tagged_expr = tag_expr desuggared in
  let let_n = count_let tagged_expr 0L in
  let instrs = compile_expr tagged_expr [] [] in
  let final =
    [ IPush (Reg RBP) ]
    @ [ IMov (Reg RBP, Reg RSP) ]
    @ [ ISub (Reg RSP, Const (Int64.mul 8L let_n)) ]
    @ [ IPush (Reg RDI) ]
    @ [ IMov (Reg RDI, Reg RSP) ]
    @ [ ICall (Label "set_stack_bottom") ]
    @ [ IPop (Reg RDI) ]
    @ [ IMov (Reg R15, Reg RDI) ]
    @ [ IAdd (Reg R15, Const 7L) ]
    @ [ IMov (Reg R11, Const 0xfffffffffffffff8L) ]
    @ [ IAnd (Reg R15, Reg R11) ]
    @ instrs
    @ [ IMov (Reg RSP, Reg RBP) ]
    @ [ IPop (Reg RBP) ] @ [ IRet ] @ errors
  in
  let prelude1 =
    "\nsection .text\nextern error\nextern try_gc\nextern set_stack_bottom\n"
  in
  let prelude2 = "global our_code_starts_here\nour_code_starts_here:" in
  Fmt.pf fmt "%s%s\n%s@\n%a" prelude1 externs prelude2 pp_instrs final

let compile_src =
  let _ = Str.split in
  let open Parse in
  Fmt.using (fun src -> parse_program (sexp_from_list src)) compile_prog
