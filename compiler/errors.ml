open Asm
open Funcalls

(* Label for numeric errors *)
let error_label_number =
  [ ILabel (Label "error_not_number") ]
  @ funcall "error" [ Reg RAX; Const 1L ] []

(* Label for boolean errors*)
let error_label_boolean =
  [ ILabel (Label "error_not_boolean") ]
  @ funcall "error" [ Reg RAX; Const 2L ] []

let error_label_tuple =
  [ ILabel (Label "error_not_tuple") ]
  @ funcall "error" [ Reg RAX; Const 3L ] []

let error_label_index_too_low =
  [ ILabel (Label "error_index_too_low") ]
  @ funcall "error" [ Reg RAX; Const 4L ] []

let error_label_index_too_high =
  [ ILabel (Label "error_index_too_high") ]
  @ funcall "error" [ Reg RAX; Const 5L ] []

let error_label_wrong_arity =
  [ ILabel (Label "error_wrong_arity") ]
  @ funcall "error" [ Reg RAX; Const 6L ] []

let error_label_closure =
  [ ILabel (Label "error_not_closure") ]
  @ funcall "error" [ Reg RAX; Const 7L ] []

let errors =
  error_label_number @ error_label_boolean @ error_label_tuple
  @ error_label_index_too_low @ error_label_index_too_high
  @ error_label_wrong_arity @ error_label_closure

(* Checks the provided arg to be a number *)
let check_number (arg : arg) : instruction list =
  [ IPush (Reg RAX) ]
  @ [ IMov (Reg RAX, arg) ]
  @ [ ITest (Reg RAX, Const 1L) ]
  @ [ IJnz (Label "error_not_number") ]
  @ [ IPop (Reg RAX) ]

(* Checks the value at RAX registers to be a number *)
let check_number_rax =
  [ ITest (Reg RAX, Const 1L) ] @ [ IJnz (Label "error_not_number") ]

(* Checks the value at the provided register to be a boolean *)
let check_boolean (arg : arg) : instruction list =
  [ IPush (Reg RAX) ]
  @ [ IMov (Reg RAX, arg) ]
  @ [ ITest (Reg RAX, Const 0x111L) ]
  @ [ IJz (Label "error_not_boolean") ]
  @ [ IPop (Reg RAX) ]

(* Checks the value at RAX register to be a boolean *)
let check_boolean_rax =
  [ ITest (Reg RAX, Const 0x111L) ] @ [ IJz (Label "error_not_boolean") ]

(* Checks the value of a register to be a tuple *)
let check_pair (arg : arg) : instruction list =
  [ IPush (Reg RAX) ]
  @ [ IMov (Reg RAX, arg) ]
  @ [ ITest (Reg RAX, Const 1L) ]
  @ [ IJz (Label "error_not_tuple") ]
  @ [ IPop (Reg RAX) ]

(* Checks the value of the RAX register to be a tuple *)
let check_pair_rax =
  [ ITest (Reg RAX, Const 1L) ] @ [ IJz (Label "error_not_tuple") ]

(* Check that the arity given is equal to the arity stored in a closure *)
let check_arity_rax (n : int64) : instruction list =
  [ ISub (Reg RAX, Const 0x5L) ]
  @ [ IMov (Reg R11, RegOffset (RAX, 0L)) ]
  @ [ ICmp (Reg R11, Const n) ]
  @ [ IJne (Label "error_wrong_arity") ]
  @ [ IAdd (Reg RAX, Const 0x5L) ]

(* Check that a value in RAX is a closure *)
let check_closure =
  [ ITest (Reg RAX, Const 5L) ] @ [ IJz (Label "error_not_closure") ]
