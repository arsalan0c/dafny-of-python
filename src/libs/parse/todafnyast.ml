open Base

open Astpy
open Astdfy

let printf = Stdlib.Printf.printf

exception DfyAstError of string
let[@inline] failwith msg = raise (DfyAstError msg)

let typ_idents = Hash_set.create (module String)

let rec type_dfy = function 
  | IdentTyp s -> DIdentTyp s
  | Int s -> DInt s
  | Float s -> DReal s 
  | Bool s -> DBool s
  | Str s -> DString s
  | Non _ -> DVoid
  | List(s, ot) -> begin
    match ot with
    | Some t -> let r = type_dfy t in DSeq(s, r)
    | None -> failwith "Please specify the exact sequence type"
    end
  | Set(s, ot) -> begin
    match ot with
    | Some t -> let r = type_dfy t in DSet(s, r)
    | None -> failwith "Please specify the exact set type"
    end
  | Dict(s, ot1, ot2) -> begin
    match ot1, ot2 with
    | Some t1, Some t2 -> let r1 = type_dfy t1 in let r2 = type_dfy t2 in DMap (s, r1, r2)
    | None, _ -> failwith "Please specify the exact map type"
    | _, None -> failwith "Please specify the exact map type"

    end
  | Tuple(s, olt) -> begin
    match olt with
    | Some lt -> DTuple (s, (List.map ~f:type_dfy lt))
    | None -> failwith "Please specify the exact tuple type"
    end

let ident_dfy = function
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
  | Tuple el -> DSeqExpr (List.map ~f:exp_dfy el)  
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
  | Typ _ -> failwith "type in expression context only allowed as right-hand-side of assignment"

let spec_dfy = function
  | Pre c -> DRequires (exp_dfy c)
  | Post c -> DEnsures (exp_dfy c)
  | Invariant e -> DInvariant (exp_dfy e)
  | Decreases d -> DDecreases (exp_dfy d)

let param_dfy = function
  | Param(id, t) -> ((ident_dfy id), (type_dfy t))

let rec stmt_dfy = function
  | Exp(Call(id, el)) -> 
    let d_el = List.map ~f:exp_dfy el in
    DCallStmt(id, d_el)
  | Assign(il, el) -> DAssign (List.map ~f:ident_dfy il, (List.map ~f:exp_dfy el))
  | IfElse(e, sl1, esl, sl3) -> let d_esl = List.map ~f:(fun (e,sl) -> let d_e = exp_dfy e in (d_e, (List.map ~f:stmt_dfy sl))) esl in DIf(exp_dfy e, (List.map ~f:stmt_dfy sl1), d_esl, (List.map ~f:stmt_dfy sl3))
  | Return el -> DReturn (List.map ~f:exp_dfy el)
  | Assert e -> DAssert (exp_dfy e)
  | Break -> DBreak
  | Continue -> failwith "continue statements are not supported"
  | Pass -> DEmptyStmt
  | While(e, speclst, sl) -> DWhile (exp_dfy e, List.map ~f:spec_dfy speclst, List.map ~f:stmt_dfy sl)
  | Function _  -> assert false
  | Exp(_) -> failwith "non-call expressions are not allowed as statements"

let is_toplevel = function
  | Function _ -> true
  | Assign _ -> true
  | _ -> false

let not_toplevel = function
  | Function _ -> false
  | _ -> true

let toplevel_dfy = function
  | Function(speclst, i, pl, t, sl) -> 
    let tl = begin
      match type_dfy t with
      | DTuple(_, tl) -> tl
      | t -> [t]
      end 
    in [Some (DMeth (List.map ~f:spec_dfy speclst, i, List.map ~f:param_dfy pl, tl, List.map ~f:stmt_dfy sl))]
  | Assign(il, el) ->
      let convert_typsyn ident typ = 
        let s_ident = Sourcemap.segment_value ident in
        match typ with
        | Typ t -> Hash_set.add typ_idents s_ident; Some (DTypSynonym (ident_dfy ident, type_dfy t))
        | Identifier typ_ident -> begin 
            let s_typ = Sourcemap.segment_value typ_ident in
            match Base.Hash_set.find typ_idents ~f:(fun s -> String.compare s s_typ = 0) with
            | Some _ -> Hash_set.add typ_idents s_ident; Some (DTypSynonym (ident_dfy ident, type_dfy (IdentTyp typ_ident)))
            | None -> None
          end
        | _ -> None
      in begin
        match List.map2 ~f:convert_typsyn il el with
        | Ok typ_syns -> typ_syns
        | Unequal_lengths -> failwith "Number of left-hand identifiers must be equal to number of right-hand expressions"
      end
  | _ -> [None]

let prog_dfy = function
  | Program sl -> 
    let toplevel_stmts = List.filter ~f:is_toplevel sl in
    let d_toplevel_stmts_o = List.fold ~f:(fun so_far s -> so_far@(toplevel_dfy s)) ~init:[] toplevel_stmts in
    let d_toplevel_stmts = List.filter_map ~f:(fun x -> x) d_toplevel_stmts_o in
    let non_toplevel_stmts = List.filter ~f:(fun x -> not_toplevel x) sl in
    let d_non_toplevel_stmts = List.map ~f:stmt_dfy non_toplevel_stmts in
    let main = DMeth([DNone], (Lexing.dummy_pos, Some "Main"), [], [DVoid], d_non_toplevel_stmts) in
    DProg("pyny", d_toplevel_stmts@[main])
