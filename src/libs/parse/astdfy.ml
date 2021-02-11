(* open Sourcemap

type dOperator = 
  | DEq of segment
  | DNeq of segment
  | DPlus of segment
  | DMinus of segment
  | DTimes of segment
  | DLt of segment
  | DLEq of segment
  | DGt of segment
  | DGEq of segment
  | DAnd of segment 
  | DOr of segment
  | DImplies of segment
  | DExplies of segment
  [@@deriving sexp]

type dExpr = 
  | DIdentifier of segment
  | DFalse
  | DTrue
  | DNull
  | DThis
  | DFresh
  | DOld
  | DChar of string
  | DInt of int
  | DReal of float
  | DString of string
  | DBinary of dExpr * dOperator * dExpr
  | DUnary of dOperator  * dExpr
  (* | DCall of 
  | DRel of  *)
  [@@deriving sexp]

type dStmt = 
  | DIf of dExpr * dStmtLst * dStmtLst
  | DAssume of dExpr
  | DAssign of string * dExpr
  | DWhile of dExpr * dExpr list * dStmtLst
  | DAssert of dExpr
  | DPrint of dExpr
  | DForall of dExpr * dExpr
  | DReturn of dExpr
  | DBreak
  | DContinue 
  | DYield
  | DCall of string * dExpr list

and dStmtLst =
  | DBlock of dStmt list
  [@@deriving sexp]

type dType = DInt | DFloat | DBool | DString | DChar | DSeq | DArray
[@@deriving sexp]
type dParam = string * dType (* name: type *)
[@@deriving sexp]

type dSpec = dExpr * dExpr
[@@deriving sexp]

type dMethod =
  DMeth of string * dParam list * dParam list * dSpec * dStmtLst
  [@@deriving sexp]

type dProgram =
  DProg of string * dMethod list
  [@@deriving sexp]







 *)
