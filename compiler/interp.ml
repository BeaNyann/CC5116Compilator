(** Values *)

type value = NumV of int64

(* Lifting functions on int to values *)
let liftNumV (op : int64 -> int64) : value -> value = function
  | NumV n -> NumV (op n)

open Expr

(** Interpreter *)

let interp (e : 'a expr) : value =
  match e with Num (n, _) -> NumV n | _ -> NumV 1L

(* | Add1 (e,_) -> liftNumV (Int64.add 1L) (interp e)
   | Sub1 (e,_) -> liftNumV (Int64.add (-1L)) (interp e) *)

(** Pretty printing **)

(* printing values and expressions *)
let pp_value : value Fmt.t =
 fun fmt e -> match e with NumV n -> Fmt.int64 fmt n
