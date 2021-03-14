open Base
open Sexplib.Std

open Sourcemap

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
  | DIdentTyp of segment
  | DInt of segment
  | DReal of segment 
  | DBool of segment  
  | DString of segment  
  | DChar of segment 
  | DSeq of segment * dTyp
  | DSet of segment * dTyp
  | DMap of segment * dTyp * dTyp
  | DArray of segment * dTyp
  | DVoid
  | DTuple of segment * dTyp list
  [@@deriving sexp]

type dParam = dId * dTyp (* name: type *)
[@@deriving sexp]

type dExpr = 
  | DIdentifier of segment
  | DNull
  | DThis
  | DFresh
  | DOld of segment * dExpr
  | DIntLit of int
  | DRealLit of float
  | DBoolLit of bool
  | DStringLit of string
  | DBinary of dExpr * dOp * dExpr
  | DUnary of dOp * dExpr
  | DCallExpr of dId * dExpr list
  | DSeqExpr of dExpr list
  | DSetExpr of dExpr list
  | DMapExpr of (dExpr * dExpr) list
  | DArrayExpr of dExpr list
  | DSubscript of dExpr * dExpr (* value, slice *)
  | DSlice of dExpr option * dExpr option (* lower, upper *)
  | DForall of dId list * dExpr
  | DExists of dId list * dExpr
  | DLen of segment * dExpr
  | DLambda of dParam list * dSpec list * dExpr

and dSpec = 
  | DRequires of dExpr 
  | DEnsures of dExpr
  | DNone
  | DInvariant of dExpr
  | DDecreases of dExpr
  (* | DFresh of dExpr *)
  (* | DOld of dExpr *)
  [@@deriving sexp]

type dStmt = 
  | DEmptyStmt
  | DAssume of dExpr
  | DAssert of dExpr
  | DAssign of dTyp * dId list * dExpr list
  | DIf of dExpr * dStmt list * (dExpr * dStmt list) list * dStmt list
  | DWhile of dExpr * dSpec list * dStmt list
  | DReturn of dExpr list
  | DBreak
  | DCallStmt of dId * dExpr list
  [@@deriving sexp]

type dTopLevel = 
  | DMeth of dSpec list * dId * dParam list * dTyp list * dStmt list
  | DTypSynonym of dId * dTyp
  [@@deriving sexp]

type dProgram = DProg of string * dTopLevel list
[@@deriving sexp]
