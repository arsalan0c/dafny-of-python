open Base
open Sexplib.Std
open Sourcemap

exception PyAstError of string
let[@inline] failwith msg = raise (PyAstError msg)

type typ =
  | TIdent of segment
  | TInt of segment
  | TFloat of segment 
  | TBool of segment 
  | TStr of segment  
  | TNone of segment
  | TObj of segment
  | TLst of segment * typ option
  | TDict of segment * typ option * typ option
  | TSet of segment * typ option
  | TTuple of segment * (typ list) option
  | TCallable of segment * (typ list) * typ (* args, return *)
  | TType of segment * typ option
  (* | Union of segment * typ list *)
  [@@deriving sexp]

let typ_plus = [TInt def_seg; TFloat def_seg; TStr def_seg; TLst (def_seg, None)]
let typ_minus = [TInt def_seg; TFloat def_seg]
let typ_times = [TInt def_seg; TFloat def_seg]
let typ_divide = [TInt def_seg; TFloat def_seg]
let typ_mod = [TInt def_seg]
let typ_rel = [TInt def_seg; TFloat def_seg]
let typ_in = [TLst (def_seg, None)]

let rec subtyp pt1 pt2 = 
  let o_compare ot1 ot2 = match ot1, ot2 with
  | Some t1, Some t2 -> subtyp t1 t2
  | None, None -> true
  | _, _ -> false
  in 
  match pt1, pt2 with
  | TIdent _, TIdent _ -> false
  | TInt _, TInt _ -> true
  | TFloat _, TFloat _ -> true
  | TInt _, TFloat _ -> true
  | TBool _, TBool _ -> true
  | TStr _, TStr _ -> true
  | TNone _, TNone _ -> true
  | TLst _, TLst (_, None) -> true
  | TLst (_, ot1), TLst (_, ot2) -> o_compare ot1 ot2
  | TDict (_, ot1, ot3), TDict (_, ot2, ot4) -> (o_compare ot1 ot2) && (o_compare ot3 ot4)
  | TSet (_, ot1), TSet (_, ot2) -> o_compare ot1 ot2
  | TTuple (_, otl1), TTuple (_, otl2) -> begin
    match otl1, otl2 with
    | Some tl1, Some tl2 -> 
      let c = List.compare (fun x y -> if subtyp x y then 0 else -1) tl1 tl2 in
      if c = 0 then true else false
    | None, None -> true
    | _, _ -> false
    end
  | _, _ -> false

let eqtyp tp1 tp2 = (subtyp tp1 tp2) && (subtyp tp2 tp1)
let either_subtyp tp1 tp2 = (subtyp tp1 tp2) || (subtyp tp2 tp1)

type identifier = segment
[@@deriving sexp]

type unaryop = Not of segment | UMinus of segment
[@@deriving sexp]

type literal = TrueLit | FalseLit | IntLit of string | FloatLit of string | StringLit of string | NoneLit
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
  | NotIn of segment
  | In of segment
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
  | Max of segment * exp
  | Old of segment * exp
  | Fresh of segment * exp
  | Lambda of identifier list * exp
  | IfElseExp of exp * exp * exp
  [@@deriving sexp]

type param = identifier * exp (* name: type *)
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
  | For of spec list * identifier list * exp * stmt list
  | While of spec list * exp * stmt list
  | Assign of exp option * exp list * exp list (* type, lhs, rhs *)
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

let rec idlst_to_id = function
  | [] -> []
  | (Identifier x)::ys -> x::(idlst_to_id ys)
  | _ -> failwith "expected identifier"