open Base
open Sexplib.Std
open Sourcemap

exception PyAstError of string
let[@inline] failwith msg = raise (PyAstError msg)

type literal = BoolLit of bool | IntLit of int | FloatLit of float | StringLit of string | NonLit
[@@deriving sexp]

type typ =
  | TIdent of segment
  | TInt of segment
  | TFloat of segment 
  | TBool of segment 
  | TStr of segment  
  | TNone of segment
  | TLst of segment * typ option
  | TDict of segment * typ option * typ option
  | TSet of segment * typ option
  | TTuple of segment * (typ list) option
  | TCallable of segment * (typ list) * typ (* args, return *)
  (* | Union of segment * typ list *)
  [@@deriving sexp]

let rec typ_compare pt1 pt2 = 
  let o_compare ot1 ot2 = match ot1, ot2 with
  | Some t1, Some t2 -> typ_compare t1 t2
  | None, None -> 0
  | _, _ -> -1
  in 
  match pt1, pt2 with
  | TIdent id1, TIdent id2 -> segment_values_compare id1 id2
  | TInt i1, TInt i2 -> segment_values_compare i1 i2
  | TFloat f1, TFloat f2 -> segment_values_compare f1 f2
  | TBool b1, TBool b2 -> segment_values_compare b1 b2
  | TStr s1, TStr s2 -> segment_values_compare s1 s2
  | TNone _, TNone _ -> 0
  | TLst (_, ot1), TLst (_, ot2) -> o_compare ot1 ot2
  | TDict (_, ot1, ot3), TDict (_, ot2, ot4) -> (o_compare ot1 ot2) + (o_compare ot3 ot4)
  | TSet (_, ot1), TSet (_, ot2) -> o_compare ot1 ot2
  | TTuple (_, otl1), TTuple (_, otl2) -> begin
    match otl1, otl2 with
    | Some tl1, Some tl2 -> List.compare typ_compare tl1 tl2
    | None, None -> 0
    | _, _ -> -1
    end
  | _, _ -> -1

type identifier = segment
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
  | Typ of typ
  | Literal of literal
  | Identifier of identifier
  | Dot of exp * identifier
  | BinaryExp of exp * binaryop * exp
  | UnaryExp of unaryop * exp
  | Call of exp * exp list
  | Lst of exp list
  | Array of exp list
  | Set of exp list
  (* | SetComp of exp * exp * exp * exp list result, target, domain, conditions *)
  | Dict of (exp * exp) list
  | Tuple of exp list
  | Subscript of exp * exp (* value, slice *)
  | Index of exp
  | Slice of exp option * exp option (* lower, upper *)
  | Forall of identifier list * exp
  | Exists of identifier list * exp
  | Len of segment * exp
  | Old of segment * exp
  | Fresh of segment * exp
  | Lambda of identifier list * exp
  | IfElseExp of exp * exp * exp
  [@@deriving sexp]

type param = Param of identifier * exp (* name: type *)
[@@deriving sexp]

type spec = 
  | Pre of exp 
  | Post of exp 
  | Invariant of exp
  | Decreases of exp
  | Reads of exp
  | Modifies of exp
  [@@deriving sexp]

type stmt =
  | IfElse of exp * stmt list * (exp * stmt list) list * stmt list
  | For of spec list * exp list * exp * stmt list
  | While of spec list * exp * stmt list
  | Assign of exp option * exp list * exp list
  | Function of spec list * identifier * param list * exp * stmt list (* spec, name, params, return type, body *)
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
