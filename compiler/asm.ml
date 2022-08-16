(* registers *)
type reg = RAX | RSP | RBP | RDI | RSI | RDX | RCX | R8 | R9 | R11 | R15

(* arguments for instructions *)
type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int64
  | Label of string

(* asm instructions *)
type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IDiv of arg
  | IImul of arg * arg
  | ICmp of arg * arg
  | ISar of arg * arg
  | Ilsl of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg
  | ITest of arg * arg
  | ICall of arg
  | ICqo
  | ILabel of arg
  | IJmp of arg
  | IJe of arg
  | IJne of arg
  | IJl of arg
  | IJle of arg
  | IJg of arg
  | IJge of arg
  | IJb of arg
  | IJbe of arg
  | IJz of arg
  | IJnz of arg
  | IJo of arg
  | Ijno of arg
  | IPush of arg
  | IPop of arg
  | IComment of arg
  | IRet

let pp_str = Fmt.string

let pp_int = Fmt.int

let pp_bool = Fmt.bool

let pp_reg : reg Fmt.t =
 fun fmt r ->
  match r with
  | RAX -> Fmt.string fmt "RAX"
  | R11 -> Fmt.string fmt "R11"
  | RSP -> Fmt.string fmt "RSP"
  | RBP -> Fmt.string fmt "RBP"
  | RDI -> Fmt.string fmt "RDI"
  | RSI -> Fmt.string fmt "RSI"
  | RDX -> Fmt.string fmt "RDX"
  | RCX -> Fmt.string fmt "RCX"
  | R8 -> Fmt.string fmt "R8"
  | R9 -> Fmt.string fmt "R9"
  | R15 -> Fmt.string fmt "R15"

let pp_arg : arg Fmt.t =
 fun fmt arg ->
  match arg with
  | Const n -> Fmt.pf fmt "%#Lx" n
  | Reg r -> pp_reg fmt r
  | RegOffset (r, i) -> Fmt.pf fmt "[%a + 8*%#Lx]" pp_reg r i
  | Label l -> pp_str fmt l

let pp_instr : instruction Fmt.t =
 fun fmt instr ->
  match instr with
  | IMov (a1, a2) -> Fmt.pf fmt "  mov %a, %a" pp_arg a1 pp_arg a2
  | IAdd (a1, a2) -> Fmt.pf fmt "  add %a, %a" pp_arg a1 pp_arg a2
  | ISub (a1, a2) -> Fmt.pf fmt "  sub %a, %a" pp_arg a1 pp_arg a2
  | IDiv a -> Fmt.pf fmt "  idiv %a" pp_arg a
  | IImul (a1, a2) -> Fmt.pf fmt "  imul %a, %a" pp_arg a1 pp_arg a2
  | ICmp (a1, a2) -> Fmt.pf fmt "  cmp %a, %a" pp_arg a1 pp_arg a2
  | ISar (a1, a2) -> Fmt.pf fmt "  sar %a, %a" pp_arg a1 pp_arg a2
  | Ilsl (a1, a2) -> Fmt.pf fmt "  sal %a, %a" pp_arg a1 pp_arg a2
  | IAnd (a1, a2) -> Fmt.pf fmt "  and %a, %a" pp_arg a1 pp_arg a2
  | IOr (a1, a2) -> Fmt.pf fmt "  or %a, %a" pp_arg a1 pp_arg a2
  | IXor (a1, a2) -> Fmt.pf fmt "  xor %a, %a" pp_arg a1 pp_arg a2
  | ITest (a1, a2) -> Fmt.pf fmt "  test %a, %a" pp_arg a1 pp_arg a2
  | ICall a -> Fmt.pf fmt "  call %a" pp_arg a
  | ICqo -> Fmt.pf fmt "  cqo"
  | ILabel l -> Fmt.pf fmt "%a:" pp_arg l
  | IJmp l -> Fmt.pf fmt "  jmp %a" pp_arg l
  | IJe l -> Fmt.pf fmt "  je %a" pp_arg l
  | IJne l -> Fmt.pf fmt "  jne %a" pp_arg l
  | IJl l -> Fmt.pf fmt "  jl %a" pp_arg l
  | IJle l -> Fmt.pf fmt "  jle %a" pp_arg l
  | IJg l -> Fmt.pf fmt "  jg %a" pp_arg l
  | IJge l -> Fmt.pf fmt "  jge %a" pp_arg l
  | IJb l -> Fmt.pf fmt "  jb %a" pp_arg l
  | IJbe l -> Fmt.pf fmt "  jbe %a" pp_arg l
  | IJz l -> Fmt.pf fmt "  jz %a" pp_arg l
  | IJnz l -> Fmt.pf fmt "  jnz %a" pp_arg l
  | IJo l -> Fmt.pf fmt "  jo %a" pp_arg l
  | Ijno l -> Fmt.pf fmt "  jno %a" pp_arg l
  | IPush a -> Fmt.pf fmt "  push %a" pp_arg a
  | IPop a -> Fmt.pf fmt "  pop %a" pp_arg a
  | IComment s -> Fmt.pf fmt ";%a" pp_arg s
  | IRet -> Fmt.pf fmt "  ret"

let pp_instrs : instruction list Fmt.t =
  Fmt.list ~sep:Format.pp_force_newline pp_instr
