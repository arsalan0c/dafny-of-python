open Base

type binaryop = Plus | PlusEq | Minus | MinusEq | Times | TimesEq | Divide | DivideEq | Mod | Eq | EqEq | NEq | Lt | LEq | Gt | GEq | And | Or
[@@deriving sexp]

type unaryop = Not | Neg
[@@deriving sexp]

type identifier = Identifier of string
[@@deriving sexp]

type literal = BooleanLiteral of bool | IntegerLiteral of int | StringLiteral of string
[@@deriving sexp]

type atom = string
[@@deriving sexp]

type exp =
  | Atom of atom 
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
  | If of (exp * stmt list * stmt list)
  | While of (exp * stmt list)
  | Assignment of (atom list * exp list)
  | Function of (identifier * atom list * stmt list)
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

let binaryop_string = function
  | Plus -> "+"
  | PlusEq -> "+="
  | Minus -> "-"
  | MinusEq -> "-="
  | Times -> "*"
  | TimesEq -> "*="
  | Divide -> "/"
  | DivideEq -> "/="
  | Mod -> "%"
  | NEq -> "!="
  | Eq -> "="
  | EqEq -> "=="
  | Lt -> "<"
  | LEq -> "<="
  | Gt -> ">"
  | GEq -> ">="
  | And -> "and"
  | Or -> "or"

let unaryop_string = function
  | Not -> "not"
  | Neg -> "-"

let literal_string = function
  | BooleanLiteral(b) -> Bool.to_string b
  | IntegerLiteral(i) -> Int.to_string i 
  | StringLiteral(s) -> s
