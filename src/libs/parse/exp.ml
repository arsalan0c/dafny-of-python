open Base

type binaryop = Plus | PlusEq | Minus | MinusEq | Times | TimesEq | Divide | DivideEq | Mod | Eq | EqEq | NEq | Lt | LEq | Gt | GEq | And | Or

type unaryop = Not | Neg

type identifier = Identifier of string

type literal = BooleanLiteral of bool | IntegerLiteral of int | StringLiteral of string

type atom = string

type exp =
  | Atom of atom
  | ExpList of exp list
  | BinaryOp of (exp * binaryop * exp)
  | UnaryOp of (unaryop * exp)
  | Literal of literal
  | Call of (exp * exp list)

type arg = Arg of exp

type stmt =
  | Newline of string
  | If of (exp * stmt list * stmt list)
  | While of (exp * stmt list)
  | Assignment of (atom list * exp list)
  | Function of (identifier * atom list * stmt list)
  | ReturnExp
  | Return of exp
  | Break
  | Continue
  | Print of exp
  | Exp of exp
  | StmtList of stmt list

type sexp =
  | Program of stmt list

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



