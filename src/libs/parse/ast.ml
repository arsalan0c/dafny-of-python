open Base
open List

exception AstError of string

(* let var_counter = 0 *)

let rec replicate_str s n = match n with
| 0 -> ""
| n -> s ^ replicate_str s (n - 1)

let space = " "

let indent i = replicate_str space i

let[@inline] failwith msg = raise (AstError msg)

type literal = BooleanLiteral of bool | IntegerLiteral of int | StringLiteral of string | NoneLiteral
[@@deriving sexp]

type identifier = Identifier of string
[@@deriving sexp]

type unaryop = Not | UMinus
[@@deriving sexp]

type binaryop = Plus | Minus | Times | Divide | Mod | Eq | EqEq | NEq | Lt | LEq | Gt | GEq | And | Or
[@@deriving sexp]

type exp =
  | Identifier of string
  | BinaryOp of (exp * binaryop * exp)
  | UnaryOp of (unaryop * exp)
  | Literal of literal
  | Call of (exp * exp list)
[@@deriving sexp]

type arg = Arg of exp
[@@deriving sexp]

type stmt =
  | IfElse of (exp * stmt list * stmt list)
  | While of (exp * stmt list)
  | Assign of (identifier list * exp list)
  | Function of (identifier * identifier list * stmt list)
  | Return of exp
  | Assert of exp
  | Break
  | Continue
  | Print of exp
  | Exp of exp
[@@deriving sexp]

type sexp =
  | Program of stmt list
[@@deriving sexp]

let id_str id: identifier -> 'a = function
  | Identifier(ident) -> (indent id) ^ ident

let literal_str id = function
  | BooleanLiteral(b) -> (indent id) ^ Bool.to_string b
  | IntegerLiteral(i) -> (indent id) ^ Int.to_string i 
  | StringLiteral(s) -> (indent id) ^ s
  | NoneLiteral -> "null"

let unaryop_str = function
| Not -> "!"
| UMinus -> "-"

let binaryop_str = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "%"
  | NEq -> "!="
  | Eq -> "="
  | EqEq -> "=="
  | Lt -> "<"
  | LEq -> "<="
  | Gt -> ">"
  | GEq -> ">="
  | And -> "&&"
  | Or -> "||"

let rec exp_str id = function
  | Identifier(ident) -> (indent id) ^ ident
  | BinaryOp(e1, op, e2) -> (indent id) ^ String.concat ~sep:" " [(exp_str 0 e1); (binaryop_str op); (exp_str 0 e2)]
  | UnaryOp(op, e) -> (indent id) ^ (unaryop_str op) ^ (exp_str 0 e)
  | Literal(l) -> (indent id) ^ literal_str 0 l
  | Call(e, el) -> (indent id) ^ (exp_str 0 e) ^ "(" ^ (String.concat ~sep:", " (map ~f:(exp_str 0) el)) ^ ")"

let rec stmt_str id = function
  | Exp(_) -> "" (* (indent id) ^ "var temp" ^ Int.to_string var_counter ^ " := " ^ exp_str 0 e ^ ";" *)
  | Assign(idl, el) -> (indent id) ^ (String.concat ~sep:", " (map ~f:(id_str 0) idl)) ^ " := " ^ (String.concat ~sep:", " (map ~f:(exp_str 0) el)) ^ ";"
  | IfElse(e, sl1, sl2) -> (indent id) ^ "if " ^ (exp_str 0 e) ^ " {\n" ^ 
    (String.concat ~sep:("\n") (map ~f:(stmt_str (id+2)) sl1)) ^ 
    (if length sl2 > 0 then "\n" ^ indent id ^ "} else {\n" ^ (String.concat ~sep:("\n") (map ~f:(stmt_str (id+2)) sl2)) else "") ^ "\n" ^ (indent id) ^ "}"
  | Return(e) -> (indent id) ^ "return " ^ exp_str 0 e ^ ";"
  | Assert(e) -> (indent id) ^ "assert " ^ exp_str 0 e ^ ";"
  | While(e, sl) -> (indent id) ^ "while " ^ exp_str 0 e ^ " {\n" ^ 
    (String.concat ~sep:("\n") (map ~f:(stmt_str (id+2)) sl)) ^ "\n" ^ (indent id) ^ "}"
  | _ -> failwith "unsupported AST node"


let is_fn = function
  | Function(_, _, _) -> true
  | _ -> false

let prog_str = function
  | Program(sl) -> let non_fn_stmts = filter ~f:(fun x -> not (is_fn x)) sl in
      let fn_stmts = filter ~f:is_fn sl in
      "method Main() {\n" ^ (String.concat ~sep:"\n" (map ~f:(stmt_str 2) non_fn_stmts)) ^ "\n}\n\n" ^
      (String.concat ~sep:"\n" (map ~f:(stmt_str 2) fn_stmts))

