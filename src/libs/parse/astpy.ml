open Base
open Sexplib.Std
open Sourcemap

exception PyAstError of string
let[@inline] failwith msg = raise (PyAstError msg)

type literal = BoolLit of bool | IntLit of int | FloatLit of float | StringLit of string | NonLit
[@@deriving sexp]

type pytype =
  | Void
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

let rec pytype_compare pt1 pt2 = 
  let o_compare ot1 ot2 = match ot1, ot2 with
  | Some t1, Some t2 -> pytype_compare t1 t2
  | None, None -> 0
  | _, _ -> -1
  in 
  match pt1, pt2 with
  | Void, Void -> 0
  | IdentTyp id1, IdentTyp id2 -> segment_values_compare id1 id2
  | Int i1, Int i2 -> segment_values_compare i1 i2
  | Float f1, Float f2 -> segment_values_compare f1 f2
  | Bool b1, Bool b2 -> segment_values_compare b1 b2
  | Str s1, Str s2 -> segment_values_compare s1 s2
  | Non _, Non _ -> 0
  | LstTyp (_, ot1), LstTyp (_, ot2) -> o_compare ot1 ot2
  | Dict (_, ot1, ot3), Dict (_, ot2, ot4) -> (o_compare ot1 ot2) + (o_compare ot3 ot4)
  | Set (_, ot1), Set (_, ot2) -> o_compare ot1 ot2
  | Tuple (_, otl1), Tuple (_, otl2) -> begin
    match otl1, otl2 with
    | Some tl1, Some tl2 -> List.compare pytype_compare tl1 tl2
    | None, None -> 0
    | _, _ -> -1
    end
  | _, _ -> -1

type identifier = segment
[@@deriving sexp]

type param = Param of identifier * pytype
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
  | Lambda of identifier list * exp
  [@@deriving sexp]

type spec = 
  | Pre of exp 
  | Post of exp 
  | Invariant of exp
  | Decreases of exp
  [@@deriving sexp]

type stmt =
  | IfElse of exp * stmt list * (exp * stmt list) list * stmt list
  | While of exp * spec list * stmt list
  | Assign of pytype * identifier list * exp list
  | Function of spec list * identifier * param list * pytype * stmt list (* spec, name, params, return type, body *)
  | Return of exp
  | Assert of exp
  | Break
  | Continue
  | Pass
  | Exp of exp
  [@@deriving sexp]

type program =
  | Program of stmt list
  [@@deriving sexp]
