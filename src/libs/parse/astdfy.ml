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

type dType = 
  | DIdentType of segment
  | DInt of segment
  | DReal of segment 
  | DBool of segment  
  | DString of segment  
  | DChar of segment 
  | DSeq of segment * dType
  | DSet of segment * dType
  | DMap of segment * dType * dType
  | DVoid
  | DTuple of segment * dType list
  [@@deriving sexp]

type dParam = dId * dType (* name: type *)
[@@deriving sexp]

type dExpr = 
  | DIdentifier of segment
  | DNull
  | DThis
  | DFresh
  | DOld
  | DIntLit of int
  | DRealLit of float
  | DBoolLit of bool
  | DStringLit of string
  | DBinary of dExpr * dOp * dExpr
  | DUnary of dOp * dExpr
  | DCallExpr of dId * dExpr list
  | DSeqExpr of dExpr list
  | DSubscript of dExpr * dExpr (* value, slice *)
  | DSlice of dExpr option * dExpr option (* lower, upper *)
  | DForall of dId list * dExpr
  | DExists of dId list * dExpr
  | DLen of dExpr
[@@deriving sexp]

type dSpec = 
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
  | DAssign of dId list * dExpr list
  | DIf of dExpr * dStmt list * (dExpr * dStmt list) list * dStmt list
  | DWhile of dExpr * dSpec list * dStmt list
  | DReturn of dExpr list
  | DBreak
  | DCallStmt of dId * dExpr list
  [@@deriving sexp]

type dTopLevel = 
  | DMeth of dSpec list * dId * dParam list * dType list * dStmt list
  | DTypeSynonym of dId * dType
  [@@deriving sexp]

type dProgram = DProg of string * dTopLevel list
[@@deriving sexp]
