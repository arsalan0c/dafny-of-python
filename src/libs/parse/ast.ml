open Base

type binaryop = Plus | Minus | Times | Divide | Mod | Eq | EqEq | NEq | Lt | LEq | Gt | GEq | And | Or
[@@deriving sexp]

type unaryop = Not | UMinus
[@@deriving sexp]

type identifier = Identifier of string
[@@deriving sexp]

type literal = BooleanLiteral of bool | IntegerLiteral of int | StringLiteral of string
[@@deriving sexp]

type exp =
  | Identifier of string
  | ExpList of exp list
  | IntegerLiteral of string
  | BinaryOp of (exp * binaryop * exp)
  | UnaryOp of (unaryop * exp)
  | Literal of literal
  | Call of (exp * exp list)
[@@deriving sexp]

type arg = Arg of exp
[@@deriving sexp]

type stmt =
  | Newline of string
  | IfElse of (exp * stmt list * stmt list)
  | While of (exp * stmt list)
  | Assign of (identifier list * exp list)
  | Function of (identifier * identifier list * stmt list)
  | Return of exp
  | Break
  | Continue
  | Print of exp
  | Exp of exp
  | StmtList of stmt list
[@@deriving sexp]

type sexp =
  | Program of stmt list
[@@deriving sexp]
