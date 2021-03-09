open Base

open Astpy
open Astdfy

let printf = Stdlib.Printf.printf

exception DfyAstError of string
let[@inline] failwith msg = raise (DfyAstError msg)

let id_dfy = function
  | s -> s

let literal_dfy = function
  | BooleanLiteral b -> DBoolLit b
  | IntegerLiteral i -> DIntLit i
  | StringLiteral s -> DStringLit s
  | NoneLiteral -> DNull

let unaryop_dfy = function
  | Not s -> DNot s
  | UMinus s -> DMinus s

let binaryop_dfy = function 
  | NotIn s -> DNotIn s
  | In s -> DIn s
  | Plus s -> DPlus s
  | Minus s -> DMinus s
  | Times s -> DTimes s
  | Divide s -> DDivide s
  | Mod s -> DMod s
  | NEq s -> DNEq s
  | EqEq s -> DEq s
  | Lt s -> DLt s
  | LEq s -> DLEq s
  | Gt s -> DGt s
  | GEq s -> DGEq s
  | And s -> DAnd s
  | Or s -> DOr s
  | BiImpl s -> DBiImpl s
  | Implies s -> DImplies s
  | Explies s -> DExplies s
  
let rec exp_dfy = function
  | Identifier s -> DIdentifier s
  | BinaryOp(e1, op, e2) -> DBinary((exp_dfy e1), (binaryop_dfy op), (exp_dfy e2))
  | UnaryOp(op, e) -> DUnary((unaryop_dfy op), (exp_dfy e))
  | Literal l -> literal_dfy l
  | Call(id, el) -> let d_args = List.map ~f:exp_dfy el in DCallExpr(id, d_args)
  | Lst el -> DSeqExpr (List.map ~f:exp_dfy el)
  | Subscript(e1, e2) -> DSubscript (exp_dfy e1, exp_dfy e2)
  | Slice(e1, e2) -> begin
    match e1, e2 with
    | Some r1, Some r2 -> DSlice(Some (exp_dfy r1), Some (exp_dfy r2))
    | Some r1, None -> DSlice(Some (exp_dfy r1), None)
    | None, Some r2 -> DSlice(None, Some (exp_dfy r2))
    | None, None -> DSlice (None, None)
    end
  | Forall(s, e) -> DForall(s, exp_dfy e)
  | Exists(s, e) -> DExists(s, exp_dfy e)
  | Len e -> DLen (exp_dfy e)

let spec_dfy = function
  | Pre c -> DRequires (exp_dfy c)
  | Post c -> DEnsures (exp_dfy c)
  | Invariant e -> DInvariant (exp_dfy e)
  | Decreases d -> DDecreases (exp_dfy d)

let rec pytype_dfy = function 
  | Int s -> DInt s
  | Float s -> DReal s 
  | Bool s -> DBool s
  | Str s -> DString s
  | Non _ -> DVoid
  | List(s, ot) -> begin
    match ot with
    | Some t -> let r = pytype_dfy t in DSeq(s, Some r)
    | None -> DSeq(s, None)
    end
  | Tuple(s, olt) -> begin
    match olt with
    | Some lt -> DTuple (s, (List.map ~f:pytype_dfy lt))
    | None -> failwith "Please specify the exact tuple type"
    end
  | _ -> DVoid

let param_dfy = function
  | Param(id, t) -> ((id_dfy id), (pytype_dfy t))

let rec stmt_dfy = function
  | Exp(Call(id, el)) -> 
    let d_el = List.map ~f:exp_dfy el in
    DCallStmt(id, d_el)
  | Print e -> DPrint (exp_dfy e)
  | Assign(il, el) -> DAssign (il, (List.map ~f:exp_dfy el))
  | IfElse(e, sl1, esl, sl3) -> let d_esl = List.map ~f:(fun (e,sl) -> let d_e = exp_dfy e in (d_e, (List.map ~f:stmt_dfy sl))) esl in DIf(exp_dfy e, (List.map ~f:stmt_dfy sl1), d_esl, (List.map ~f:stmt_dfy sl3))
  | Return el -> DReturn (List.map ~f:exp_dfy el)
  | Assert e -> DAssert (exp_dfy e)
  | Break -> DBreak
  | Continue -> failwith "continue is not supported"
  | Pass -> DEmptyStmt
  | While(e, speclst, sl) -> DWhile (exp_dfy e, List.map ~f:spec_dfy speclst, List.map ~f:stmt_dfy sl)
  | Function(speclst, i, pl, t, sl) -> 
    let tl = begin
      match pytype_dfy t with
      | DTuple(_, tl) -> tl
      | t -> [t]
      end 
    in
    DMeth(List.map ~f:spec_dfy speclst, i, List.map ~f:param_dfy pl, tl, List.map ~f:stmt_dfy sl)
  | Exp(_) -> failwith "got non-call expression in statement"

let is_fn = function
  | Function(_, _, _, _, _) -> true
  | _ -> false

let prog_dfy = function
  | Program sl -> 
      (* DProg("pyny", (List.map ~f:stmt_dfy sl)) *)
      let fn_stmts = List.filter ~f:is_fn sl in
      let d_fn_stmts = List.map ~f:stmt_dfy fn_stmts in
      let non_fn_stmts = List.filter ~f:(fun x -> not (is_fn x)) sl in
      let d_non_fn_stmts = List.map ~f:stmt_dfy non_fn_stmts in
      let main = DMeth([DNone], (Lexing.dummy_pos, Some "Main"), [], [DVoid], d_non_fn_stmts) in
      DProg("pyny", main::d_fn_stmts)
