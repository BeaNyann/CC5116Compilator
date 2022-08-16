open Expr
open Asm

type env = (string * int64) list

type fenv = (string * int) list

type fcenv = (string * types list * types) list

type recordenv = (string * string list) list

type lambdaenv = (string * int64 * arg) list

(* Enviroments *)

(* lookup function for finding the slot of a value *)
let rec lookup (inf : bool) (name : string) (env : env) : int64 =
  match env with
  | [] ->
      if inf then -1L
      else Fmt.failwith "Identifier %s not found in enviroment" name
  | (n, i) :: rest -> if n = name then i else lookup inf name rest

(* lookup function for finding the arity of a function *)
let rec lookupfenv (name : string) (env : fenv) : int =
  match env with
  | [] -> Fmt.failwith "Identifier %s not found in function enviroment" name
  | (n, i) :: rest -> if n = name then i else lookupfenv name rest

(* lookup function for finding the types of the arguments and the return type of a c function *)
let rec lookupcfenv (name : string) (env : fcenv) : types list * types =
  match env with
  | [] -> Fmt.failwith "Identifier %s not found in C function enviroment" name
  | (n, args, ret_arg) :: rest ->
      if n = name then (args, ret_arg) else lookupcfenv name rest

(* lookup function for finding the fields of a record*)
let rec lookuprecordenv (name : string) (env : recordenv) : bool * string list =
  match env with
  | [] -> (false, [])
  | (n, fields) :: rest ->
      if n = name then (true, fields) else lookuprecordenv name rest

(* lookup function for finding the offset or register of a lambda argument*)
let rec lookuplambdaenv (name : string) (env : lambdaenv) : int64 * arg =
  match env with
  | [] -> Fmt.failwith "Identifier %s not found in lambda enviroment" name
  | (n, offset, register) :: rest ->
      if n = name then (offset, register) else lookuplambdaenv name rest

(* Extend-env takes an id and an enviroment and returns the extendend enviroment, with the slot used *)
let extend_env (name : string) (env : env) : env * int64 =
  let slot = Int64.add 1L (Int64.of_int (List.length env)) in
  ((name, slot) :: env, slot)

(* Extend-fenv takes an id an arity and a function enviroment and returns the extendend enviroment *)
let extend_fenv (name : string) (slot : int) (env : fenv) : fenv =
  (name, slot) :: env

(* Extend-fcenv takes an id a type list the return type and a c function enviroment and returns the extendend enviroment *)
let extend_fcenv (name : string) (args : types list) (ret_arg : types)
    (env : fcenv) : fcenv =
  (name, args, ret_arg) :: env

(* Extend-recordenv takes an id and a field list and returns the extended enviroment *)
let extend_recordenv (name : string) (fields : string list) (env : recordenv) :
    recordenv =
  (name, fields) :: env

(* Extend-lambdaenv takes an id an int and a register and returns the extended enviroment *)
let extend_lambdaenv (name : string) (offset : int64) (register : arg)
    (env : lambdaenv) : lambdaenv =
  env @ [ (name, offset, register) ]
