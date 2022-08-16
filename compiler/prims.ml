open Expr
open Asm
open Errors
open Envs

let mask_not = -9223372036854775808L

let const_true = -1L

let const_false = 9223372036854775807L

(* Compilation for primitives of 1 variable *)
let compile_prim1 (prim : prim1) (tag : int) : instruction list =
  match prim with
  | Not ->
      [ IComment (Label (Printf.sprintf "not_tag_%d" tag)) ]
      @ check_boolean_rax
      @ [ IMov (Reg R11, Const mask_not) ]
      @ [ IXor (Reg RAX, Reg R11) ]

(* Compilation of comparison primitives *)
let compile_comp (comp : comp) (tag : int) (offsetl : int64) (offsetr : int64) :
    instruction list =
  (* help defines the type of jump and the names of the Labels to be used *)
  let help (comp : comp) :
      instruction * string * string * instruction list * instruction list =
    match comp with
    | Less ->
        ( IJl (Label (Printf.sprintf "less_jump_tag_%d" tag)),
          Printf.sprintf "Less_tag_%d" tag,
          Printf.sprintf "less_jump_tag_%d" tag,
          check_number (RegOffset (RBP, Int64.mul (-1L) offsetl)),
          check_number (RegOffset (RBP, Int64.mul (-1L) offsetr)) )
    | Equal ->
        ( IJe (Label (Printf.sprintf "equal_jump_tag_%d" tag)),
          Printf.sprintf "equal_tag_%d" tag,
          Printf.sprintf "equal_jump_tag_%d" tag,
          [],
          [] )
  in
  (* Once the type of jump and labels is defined, we can use them inside a common instruction list *)
  let jump, label, jump_label, checkl, checkr = help comp in
  [ IComment (Label label) ] @ checkl @ checkr
  @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
  @ [ ICmp (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
  @ [ IMov (Reg RAX, Const const_true) ]
  @ [ jump ]
  @ [ IMov (Reg RAX, Const const_false) ]
  @ [ ILabel (Label jump_label) ]

(* in compile expr, if a prim2 expression is found, the left and right side are stored in the enviroment
  therefore, we only need to do a lookup to know their respective offset *)
let compile_prim2 (prim : prim2) (env : env) (tag : int) : instruction list =
  let offsetl = lookup false "l" env in
  let offsetr = lookup false "r" env in
  let check_num_l = check_number (RegOffset (RBP, Int64.mul (-1L) offsetl)) in
  let check_num_r = check_number (RegOffset (RBP, Int64.mul (-1L) offsetr)) in
  let check_bool_l = check_boolean (RegOffset (RBP, Int64.mul (-1L) offsetl)) in
  let check_bool_r = check_boolean (RegOffset (RBP, Int64.mul (-1L) offsetr)) in
  match prim with
  | Plus ->
      [ IComment (Label (Printf.sprintf "plus_tag_%d" tag)) ]
      @ check_num_l @ check_num_r
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
      @ [ IAdd (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
  | Minus ->
      [ IComment (Label (Printf.sprintf "minus_tag_%d" tag)) ]
      @ check_num_l @ check_num_r
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
      @ [ ISub (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
  | Times ->
      [ IComment (Label (Printf.sprintf "times_tag_%d" tag)) ]
      @ check_num_l @ check_num_r
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
      @ [ IImul (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
      @ [ ISar (Reg RAX, Const 1L) ]
  | DividedBy ->
      [ IComment (Label (Printf.sprintf "division_tag_%d" tag)) ]
      @ check_num_l @ check_num_r
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
      @ [ Ilsl (Reg RAX, Const 1L) ]
      @ [ ICqo ]
      @ [ IMov (Reg R11, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
      @ [ IDiv (Reg R11) ]
  | And ->
      [ IComment (Label (Printf.sprintf "and_tag_%d" tag)) ]
      @ check_bool_l @ check_bool_r
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
      @ [ IMov (Reg R11, Const const_false) ]
      @ [ ICmp (Reg RAX, Reg R11) ]
      @ [ IJne (Label (Printf.sprintf "and_comparison_tag_%d" tag)) ]
      @ [ IMov (Reg RAX, Const const_false) ]
      @ [ IJe (Label (Printf.sprintf "and_shortcut_tag_%d" tag)) ]
      @ [ ILabel (Label (Printf.sprintf "and_comparison_tag_%d" tag)) ]
      @ [ IAnd (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
      @ [ ILabel (Label (Printf.sprintf "and_shortcut_tag_%d" tag)) ]
  | Or ->
      [ IComment (Label (Printf.sprintf "or_tag_%d" tag)) ]
      @ check_bool_l @ check_bool_r
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
      @ [ ICmp (Reg RAX, Const const_true) ]
      @ [ IJne (Label (Printf.sprintf "or_comparison_tag_%d" tag)) ]
      @ [ IMov (Reg RAX, Const const_true) ]
      @ [ IJe (Label (Printf.sprintf "or_shortcut_tag_%d" tag)) ]
      @ [ ILabel (Label (Printf.sprintf "or_comparison_tag_%d" tag)) ]
      @ [ IOr (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
      @ [ ILabel (Label (Printf.sprintf "or_shortcut_tag_%d" tag)) ]
  | Comp comp -> compile_comp comp tag offsetl offsetr
  | Get ->
      [ IComment (Label (Printf.sprintf "get_tag_%d" tag)) ]
      @ check_num_r
      @ [ IMov (Reg R11, RegOffset (RBP, Int64.mul (-1L) offsetr)) ]
      @ [ ISar (Reg R11, Const 1L) ]
      @ [ IMov (Reg RAX, RegOffset (RBP, Int64.mul (-1L) offsetl)) ]
      @ check_pair_rax
      @ [ ISub (Reg RAX, Const 1L) ]
      @ [ ICmp (Reg R11, Const 0L) ]
      @ [ IJl (Label "error_index_too_low") ]
      @ [ ICmp (Reg R11, RegOffset (RAX, 0L)) ]
      @ [ IJge (Label "error_index_too_high") ]
      @ [ IAdd (Reg R11, Const 1L) ]
      @ [ IImul (Reg R11, Const 8L) ]
      @ [ IAdd (Reg R11, Reg RAX) ]
      @ [ IMov (Reg RAX, RegOffset (R11, 0L)) ]
