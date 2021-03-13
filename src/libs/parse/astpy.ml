open Sourcemap
open Sexplib.Std

exception PyAstError of string
let[@inline] failwith msg = raise (PyAstError msg)

type literal = BooleanLiteral of bool | IntegerLiteral of int | StringLiteral of string | NoneLiteral
[@@deriving sexp]

type pytype =
  | IdentTyp of segment
  | Int of segment
  | Float of segment 
  | Bool of segment 
  | Str of segment  
  | Non of segment
  | LstTyp of segment * pytype option
  | Dict of segment * pytype option * pytype option
  | Set of segment * pytype option
  | Tuple of segment * (pytype list) option
  [@@deriving sexp]

type identifier = segment
[@@deriving sexp]

type param = Param of (identifier * pytype)
[@@deriving sexp]

type unaryop = Not of segment | UMinus of segment
[@@deriving sexp]

type binaryop = 
  | NotIn of segment
  | In of segment
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
  | BiImpl of segment
  | Implies of segment
  | Explies of segment
  [@@deriving sexp]

type exp =
  | Identifier of segment
  | BinaryOp of (exp * binaryop * exp)
  | UnaryOp of (unaryop * exp)
  | Literal of literal
  | Call of (identifier * exp list)
  | Lst of exp list
  | Tuple of exp list
  | Subscript of exp * exp (* value, slice *)
  | Slice of exp option * exp option (* lower, upper *)
  | Forall of identifier list * exp
  | Exists of identifier list * exp
  | Len of exp
  | Typ of pytype
  [@@deriving sexp]

type spec = 
  | Pre of exp 
  | Post of exp 
  | Invariant of exp
  | Decreases of exp
  [@@deriving sexp]

type stmt =
  | IfElse of (exp * stmt list * (exp * stmt list) list * stmt list)
  | While of (exp * spec list * stmt list)
  | Assign of (identifier list * exp list)
  | Function of (spec list * identifier * param list * pytype * stmt list) (* spec, name, params, return type, body *)
  | Return of exp list
  | Assert of exp
  | Break
  | Continue
  | Pass
  | Exp of exp
  [@@deriving sexp]

type program =
  | Program of stmt list
  [@@deriving sexp]
