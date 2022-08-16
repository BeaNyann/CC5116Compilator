(** Parsing **)

(* we use CCsexp as a library for s-expressions *)
open CCSexp

(*
  Instead of using a standard algebraic data types for sexps, such as:
  
      type sexp = Atom of string | List of sexp list 
 
  this library uses a feature known as "polymorphic variants".
  This is a flexible mechanism where data is built by tagging it with symbols,
  instead of using pre-declared constructors. These symbols are written with ticks `.
  
  Then sexp is just an alias for a (recursive) polymorphic variant:

    type sexp = [ `Atom of id | `List of 'a list ] as 'a

  When matching such an sexp, we look at the tag. See the parse function below for an example.
  You can also look at the definition and implementation of the CCsexp module for more details.
*)

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse src %s: %s" src msg

let sexp_from_file (filename : string) : CCSexp.sexp =
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse file %s: %s" filename msg

let sexp_from_list (list : string) : CCSexp.sexp list =
  match CCSexp.parse_string_list list with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse file %s: %s" list msg

open Src_expr
open Expr

let rec parse (sexp : sexp) : src_expr =
  let parse_list (sexp : sexp) : (id * src_expr) list =
    match sexp with
    | `List [ `List (`Atom "tup" :: l); exp ] ->
        [
          ( "",
            Destruct
              ( List.map
                  (fun e ->
                    match e with
                    | `Atom id -> id
                    | _ -> Fmt.failwith "not a valid id")
                  l,
                parse exp ) );
        ]
    | `List [ `Atom id; exp ] -> [ (id, parse exp) ]
    | `List l ->
        List.map
          (fun bind ->
            match bind with
            | `List [ `Atom id; exp ] -> (id, parse exp)
            | `List [ `List (`Atom "tup" :: l); exp ] ->
                ( "",
                  Destruct
                    ( List.map
                        (fun e ->
                          match e with
                          | `Atom id -> id
                          | _ -> Fmt.failwith "not a valid id")
                        l,
                      parse exp ) )
            | e -> Fmt.failwith "Not a valid exp: %a" CCSexp.pp e)
          l
    | e -> Fmt.failwith "Not a valid exp: %a" CCSexp.pp e
  in
  let parse_app (l : sexp list) : src_expr =
    match List.hd l with
    | `Atom "tup" -> Tuple (List.map (fun e -> parse e) (List.tl l))
    | e -> App (parse e, List.map (fun e -> parse e) (List.tl l))
  in
  match sexp with
  | `Atom s -> (
      match Int64.of_string_opt s with
      | Some n -> Num n
      | None -> (
          match bool_of_string_opt s with
          | Some b -> Bool b
          | None -> (
              match s with
              | "DONT_CARE" -> Fmt.failwith "Reserved word: DONT_CARE"
              | _ -> Var s)))
  | `List [ `Atom "add1"; e ] -> Prim2 (Plus, parse e, Num 1L)
  | `List [ `Atom "sub1"; e ] -> Prim2 (Minus, parse e, Num 1L)
  | `List [ `Atom "not"; e ] -> Prim1 (Not, parse e)
  | `List [ `Atom "+"; e1; e2 ] -> Prim2 (Plus, parse e1, parse e2)
  | `List [ `Atom "-"; e1; e2 ] -> Prim2 (Minus, parse e1, parse e2)
  | `List [ `Atom "*"; e1; e2 ] -> Prim2 (Times, parse e1, parse e2)
  | `List [ `Atom "/"; e1; e2 ] -> Prim2 (DividedBy, parse e1, parse e2)
  | `List [ `Atom "="; e1; e2 ] -> Prim2 (Comp Equal, parse e1, parse e2)
  | `List [ `Atom "<"; e1; e2 ] -> Prim2 (Comp Less, parse e1, parse e2)
  | `List [ `Atom "and"; e1; e2 ] -> Prim2 (And, parse e1, parse e2)
  | `List [ `Atom "or"; e1; e2 ] -> Prim2 (Or, parse e1, parse e2)
  | `List [ `Atom "if"; cond; e1; e2 ] -> If (parse cond, parse e1, parse e2)
  | `List [ `Atom "let"; l; b ] -> Let (parse_list l, parse b)
  | `List [ `Atom "seq"; e1; e2 ] -> Seq (parse e1, parse e2)
  | `List [ `Atom "get"; e1; e2 ] -> Prim2 (Get, parse e1, parse e2)
  | `List [ `Atom "set"; tup; i; n ] -> Set (parse tup, parse i, parse n)
  | `List [ `Atom "lambda"; `List args; body ] ->
      Lambda
        ( List.map
            (fun e ->
              match e with
              | `Atom id -> id
              | e -> Fmt.failwith "Not a valid argument: %a" CCSexp.pp e)
            args,
          parse body )
  | `List [ `Atom "Clambda"; `Atom id; `List types; `Atom "->"; rettype ] ->
      CLambda
        ( id,
          List.map
            (fun e ->
              match e with
              | `Atom "int" -> Int
              | `Atom "bool" -> Bool
              | `Atom "any" -> Any
              | e -> Fmt.failwith "Not a valid type: %a" CCSexp.pp e)
            types,
          match rettype with
          | `Atom "int" -> Int
          | `Atom "bool" -> Bool
          | `Atom "any" -> Any
          | rettype -> Fmt.failwith "Not a valid type: %a" CCSexp.pp rettype )
  | `List l -> parse_app l

let parse_program (sexps : sexp list) : src_program =
  let parse_decl (def : sexp) : src_decl =
    match def with
    | `List [ `Atom "def"; `List l; body ] ->
        let fun_name =
          match List.hd l with
          | `Atom id -> id
          | e -> Fmt.failwith "Not a valid function name: %a" CCSexp.pp e
        in
        DFun
          ( fun_name,
            List.map
              (fun bind ->
                match bind with
                | `Atom id -> id
                | e -> Fmt.failwith "Not a valid argument: %a" CCSexp.pp e)
              (List.tl l),
            parse body )
    | `List [ `Atom "defsys"; `Atom fun_name; `List l; `Atom "->"; ret_type ] ->
        let ret_type =
          match ret_type with
          | `Atom "int" -> Int
          | `Atom "bool" -> Bool
          | `Atom "any" -> Any
          | e -> Fmt.failwith "Not a valid type: %a" CCSexp.pp e
        in
        DCFun
          ( fun_name,
            List.map
              (fun itype ->
                match itype with
                | `Atom "int" -> Int
                | `Atom "bool" -> Bool
                | `Atom "any" -> Any
                | e -> Fmt.failwith "Not a valid type: %a" CCSexp.pp e)
              l,
            ret_type )
    | `List (`Atom "record" :: `Atom id :: rest) ->
        DRecord
          ( id,
            List.map
              (fun bind ->
                match bind with
                | `Atom id -> id
                | e -> Fmt.failwith "Not a valid field: %a" CCSexp.pp e)
              rest )
    | e -> Fmt.failwith "Not a valid function declaration: %a" CCSexp.pp e
  in
  let reverse_sexps = List.rev sexps in
  let body = List.hd reverse_sexps in
  let defs = List.map parse_decl (List.rev (List.tl reverse_sexps)) in
  Program (defs, parse body)
