open Sourcemap
open Sexplib.Std

exception PyAstError of string
let[@inline] failwith msg = raise (PyAstError msg)

type literal = BooleanLiteral of bool | IntegerLiteral of int | StringLiteral of string | NoneLiteral
[@@deriving sexp]
type pytype = Type of segment
[@@deriving sexp]
type identifier = Identifier of segment
[@@deriving sexp]
type unaryop = Not of segment | UMinus of segment
[@@deriving sexp]
type param = Param of (identifier * pytype)
[@@deriving sexp]

type binaryop = 
  | Plus of segment 
  | Minus of segment
  | Times of segment
  | Divide of segment
  | Mod of segment
  | EqEq of segment
  | NEq of segment
  | Lt of segment
  | LEq of segment
  | Gt of segment
  | GEq of segment
  | And of segment
  | Or of segment
  [@@deriving sexp]

type exp =
  | Identifier of segment
  | BinaryOp of (exp * binaryop * exp)
  | UnaryOp of (unaryop * exp)
  | Literal of literal
  | Call of (exp * exp list)
  [@@deriving sexp]

type arg = Arg of exp
[@@deriving sexp]
type spec = Spec of exp * exp (* pre, post *)
[@@deriving sexp]
type stmt =
  | IfElse of (exp * stmt list * stmt list)
  | While of (exp * stmt list)
  | Assign of (identifier list * exp list)
  | Function of (spec * identifier * param list * pytype * stmt list) (* spec, name, params, return type, body *)
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
