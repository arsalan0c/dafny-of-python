open Base
(* open Sexplib.Std *)

open Pyparse.Sourcemap

type dId = segment
[@@deriving sexp]

type dOp = 
  | DNotIn of segment
  | DIn of segment
  | DEq of segment
  | DNEq of segment
  | DPlus of segment
  | DMinus of segment
  | DTimes of segment
  | DDivide of segment
  | DMod of segment
  | DLt of segment
  | DLEq of segment
  | DGt of segment
  | DGEq of segment
  | DAnd of segment 
  | DOr of segment
  | DNot of segment
  | DBiImpl of segment
  | DImplies of segment
  | DExplies of segment
  [@@deriving sexp]

type dTyp = 
  | DVoid
  | DIdentTyp of segment * dTyp option (* generic parameter *)
  | DInt of segment
  | DReal of segment 
  | DBool of segment  
  | DString of segment  
  | DChar of segment 
  | DSeq of segment * dTyp
  | DSet of segment * dTyp
  | DMap of segment * dTyp * dTyp
  | DArray of segment * dTyp
  | DTuple of segment * dTyp list
  | DFunTyp of segment * dTyp list * dTyp
  [@@deriving sexp]

type dParam = dId * dTyp (* name: type *)
[@@deriving sexp]

type dExpr = 
  | DIdentifier of dId
  | DDot of dExpr * dId
  | DEmptyExpr
  | DNull
  | DThis
  | DIntLit of int
  | DRealLit of float
  | DBoolLit of bool
  | DStringLit of string
  | DBinary of dExpr * dOp * dExpr
  | DUnary of dOp * dExpr
  | DCallExpr of dExpr * dExpr list
  | DSeqExpr of dExpr list
  | DSetExpr of dExpr list
  (* | DSetComp of dId list * dExpr * dExpr list * dExpr variables, target, conditions, result *)
  | DMapExpr of (dExpr * dExpr) list
  | DArrayExpr of dExpr list
  | DSubscript of dExpr * dExpr (* value, slice *)
  | DIndex of dExpr
  | DSlice of dExpr option * dExpr option (* lower, upper *)
  | DForall of dId list * dExpr
  | DExists of dId list * dExpr
  | DLen of segment * dExpr
  | DOld of segment * dExpr
  | DFresh of segment * dExpr
  | DLambda of dParam list * dSpec list * dExpr
  | DIfElseExpr of dExpr * dExpr * dExpr
  | DTupleExpr of dExpr list

and dSpec = 
  | DRequires of dExpr 
  | DEnsures of dExpr
  | DInvariant of dExpr
  | DDecreases of dExpr
  | DReads of dExpr
  | DModifies of dExpr
  [@@deriving sexp]

type dStmt = 
  | DEmptyStmt
  | DAssume of dExpr
  | DAssert of dExpr
  | DAssign of dTyp option * dId list * dExpr list
  | DIf of dExpr * dStmt list * (dExpr * dStmt list) list * dStmt list
  | DWhile of dSpec list * dExpr * dStmt list
  | DReturn of dExpr list
  | DBreak
  | DCallStmt of dExpr * dExpr list
  [@@deriving sexp]

type dGeneric = string
[@@deriving sexp]

type dTopLevel = 
  | DMeth of dSpec list * dId * dGeneric list * dParam list * dTyp list * dStmt list option
  | DFuncMeth of dSpec list * dId * dGeneric list * dParam list * dTyp * dExpr option
  | DTypSynonym of dId * dTyp option
  [@@deriving sexp]

type dProgram = DProg of string * dTopLevel list
[@@deriving sexp]
