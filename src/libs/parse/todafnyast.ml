open Base

open Astpy
open Astdfy

let printf = Stdlib.Printf.printf

exception ToDfyError of string
let[@inline] failwith msg = raise (ToDfyError msg)

let typ_idents = Hash_set.create (module String)

let check_exp_typ = function
  | Typ t -> t
  | Identifier s -> IdentTyp s
  | _ -> failwith "Invalid type"

let rec typ_dfy = function 
  | Void -> DVoid
  | IdentTyp s -> DIdentTyp (s, None)
  | Int s -> DInt s
  | Float s -> DReal s 
  | Bool s -> DBool s
  | Str s -> DString s
  | NonTyp _ -> DVoid
  | LstTyp (s, ot) -> begin
    match ot with
    | Some t -> let r = typ_dfy t in DIdentTyp (s, Some r)
    | None -> failwith "Please specify the exact sequence type"
    end
  | Set(s, ot) -> begin
    match ot with
    | Some t -> let r = typ_dfy t in DSet(s, r)
    | None -> failwith "Please specify the exact set type"
    end
  | Dict(s, ot1, ot2) -> begin
    match ot1, ot2 with
    | Some t1, Some t2 -> let r1 = typ_dfy t1 in let r2 = typ_dfy t2 in DMap (s, r1, r2)
    | None, _ -> failwith "Please specify the exact map type"
    | _, None -> failwith "Please specify the exact map type"

    end
  | Tuple (s, olt) -> begin
    match olt with
    | Some tpl -> 
    (* TODO: add postcondition to check number of args match number returned *)
      (* List.iter ~f:(fun t -> if (not (pytype_compare t ft = 0)) then failwith "All elements in the tuple must have the same type") lt; *)
      DTuple (s, List.map ~f:typ_dfy tpl) (* translate to sequence type instead of tuple *)
    | None -> failwith "Please specify the exact tuple type" (* TODO: allow 0 tuples *)
    end
  | Callable (s, tl, t) -> DFunTyp (s, List.map ~f:typ_dfy tl, typ_dfy t)

let ident_dfy = function
  | s -> s

let literal_dfy = function
  | BoolLit b -> DBoolLit b
  | IntLit i -> DIntLit i
  | FloatLit f -> DRealLit f
  | StringLit s -> DStringLit s
  | NonLit -> DNull

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
  | Dot (e, ident) -> DDot (exp_dfy e, ident)
  | BinaryOp (e1, op, e2) -> DBinary ((exp_dfy e1), (binaryop_dfy op), (exp_dfy e2))
  | UnaryOp (op, e) -> DUnary ((unaryop_dfy op), (exp_dfy e))
  | Literal l -> literal_dfy l
  | Call (e, el) -> let d_args = List.map ~f:exp_dfy el in DCallExpr(exp_dfy e, d_args)
  | Lst el -> DSeqExpr (List.map ~f:exp_dfy el)
  | Array el -> DArrayExpr (List.map ~f:exp_dfy el)
  | Set el -> DSetExpr (List.map ~f:exp_dfy el)
  (* | SetComp el -> DSetCompExpr (List.map ~f:exp_dfy el) *)
  | Dict eel -> DMapExpr (List.map ~f:(fun (k,v) -> (exp_dfy k, exp_dfy v)) eel)
  | Tuple (e::[]) -> exp_dfy e (* Dafny does not have 1-tuples *)
  | Tuple el -> DTupleExpr (List.map ~f:exp_dfy el)  
  | Subscript (e1, e2) -> DSubscript (exp_dfy e1, exp_dfy e2)
  | Index e -> DIndex (exp_dfy e)
  | Slice (e1, e2) -> begin
    match e1, e2 with
    | Some r1, Some r2 -> DSlice (Some (exp_dfy r1), Some (exp_dfy r2))
    | Some r1, None -> DSlice (Some (exp_dfy r1), None)
    | None, Some r2 -> DSlice (None, Some (exp_dfy r2))
    | None, None -> DSlice (None, None)
    end
  | Forall (s, e) -> DForall (s, exp_dfy e)
  | Exists (s, e) -> DExists (s, exp_dfy e)
  | Len (s, e) -> DLen (s, exp_dfy e)
  | Max (s, e) -> DCallExpr (DDot (exp_dfy e, s), [])
  | Old (s, e) -> DOld (s, exp_dfy e)
  | Fresh (s, e) -> DFresh (s, exp_dfy e)
  | Typ _ -> failwith "Type in expression context only allowed as right-hand-side of assignment"
  | Lambda (fl, e) -> let dfl = List.map ~f:(fun x -> (x, DVoid)) fl in DLambda (dfl, [], exp_dfy e)
  | IfElseExp (e1, c, e2) -> DIfElseExpr (exp_dfy c, exp_dfy e1, exp_dfy e2)

let spec_dfy = function
  | Pre c -> DRequires (exp_dfy c)
  | Post c -> DEnsures (exp_dfy c)
  | Invariant e -> DInvariant (exp_dfy e)
  | Decreases d -> DDecreases (exp_dfy d)
  | Reads e -> DReads (exp_dfy e)
  | Modifies e -> DModifies (exp_dfy e)

let param_dfy = function
  | Param (id, te) -> ((ident_dfy id), (typ_dfy (check_exp_typ te)))

let rec stmt_dfy = function
  | Exp (Call (e, el)) -> begin
      let d_el = List.map ~f:exp_dfy el in
      DCallStmt (exp_dfy e, d_el)
    end
  | Exp (Dot (e, ident)) -> DCallStmt (DDot (exp_dfy e, ident_dfy ident), [])
  | Assign (t, il, el) -> begin match t with 
    | Typ t -> DAssign (typ_dfy t, List.map ~f:exp_dfy il, (List.map ~f:exp_dfy el))
    | _ -> failwith "Invalid type of assignment"
    end
  | IfElse (e, sl1, esl, sl3) -> let d_esl = List.map ~f:(fun (e,sl) -> let d_e = exp_dfy e in (d_e, (List.map ~f:stmt_dfy sl))) esl in DIf(exp_dfy e, (List.map ~f:stmt_dfy sl1), d_esl, (List.map ~f:stmt_dfy sl3))
  | Return el -> DReturn [exp_dfy el]
  | Assert e -> DAssert (exp_dfy e)
  | Break -> DBreak
  | Continue -> failwith "continue statements are not supported"
  | Pass -> DEmptyStmt
  | While (speclst, e, sl) -> DWhile (List.map ~f:spec_dfy speclst, exp_dfy e, List.map ~f:stmt_dfy sl)
  | For _ -> failwith "for loops are not supported"
  | Function _  -> assert false
  | Exp e -> begin match e with
    | Call (e, el) -> begin
      let d_el = List.map ~f:exp_dfy el in
      DCallStmt (exp_dfy e, d_el)
    end
    | _ -> failwith "non-call expressions are not allowed as statements2"
  end



let is_toplevel = function
  | Function _ -> true
  | Assign (_, _, ((Typ _)::_)) -> true
  | _ -> false

let convert_typsyn ident rhs =
  match ident with | Identifier ident -> let ident_v = Sourcemap.segment_value ident in begin
    match rhs with 
    | Typ t -> Hash_set.add typ_idents ident_v; Some (DTypSynonym (ident_dfy ident, Some (typ_dfy t)))
    | Identifier (seg, Some syn) -> begin 
        match Base.Hash_set.find typ_idents ~f:(fun s -> String.compare s syn = 0) with
        | Some _ -> Hash_set.add typ_idents ident_v; Some (DTypSynonym (ident_dfy ident, Some (typ_dfy (IdentTyp (seg, Some syn)))))
        | None -> None
      end
    | _ -> None
    end
  | _ -> None

let toplevel_dfy generics = function
  | Function (speclst, i, pl, te, sl) -> let t = check_exp_typ te in
    [DMeth (List.map ~f:spec_dfy speclst, i, generics, List.map ~f:param_dfy pl, [typ_dfy t], List.map ~f:stmt_dfy sl)]
  | Assign (_, il, el) -> begin
    match List.map2 ~f:convert_typsyn il el with
    | Ok typ_syns -> List.filter_map ~f:(fun x -> x) typ_syns
    | Unequal_lengths -> failwith "Number of left-hand identifiers must be equal to number of right-hand expressions"
    end
  | _ -> []

let func_dfy generics = function
  | Function (speclst, i, pl, te, (Return e)::[]) -> let t = check_exp_typ te in
    [DFuncMeth (List.map ~f:spec_dfy speclst, i, generics, List.map ~f:param_dfy pl, typ_dfy t, exp_dfy e)]
  (* | Function (speclst, i, pl, te, (Exp e)::[]) -> let t = check_exp_typ te in
    [DFuncMeth (List.map ~f:spec_dfy speclst, i, generics, List.map ~f:param_dfy pl, typ_dfy t, exp_dfy e)]
  | Function (speclst, i, pl, te, Pass::[]) -> let t = check_exp_typ te in
    [DFuncMeth (List.map ~f:spec_dfy speclst, i, generics, List.map ~f:param_dfy pl, typ_dfy t, DEmptyExpr)]
  | Function (speclst, i, pl, te, []) -> let t = check_exp_typ te in
    [DFuncMeth (List.map ~f:spec_dfy speclst, i, generics, List.map ~f:param_dfy pl, typ_dfy t, DEmptyExpr)] *)
  | _ -> []  

let is_func = function
  | Function (_, _, _, _, (Return _)::[]) -> true
  (* | Function (_, _, _, _, (Exp _)::[]) -> true *)
  (* | Function (_, _, _, _, Pass::[]) -> true *)
  (* | Function (_, _, _, _, []) -> true *)
  | _ -> false

let prog_dfy p =
  let (n_p, gens) = Generics.prog p in
  let p = Convertfor.prog n_p in
  let (Program sl) = Convertlist.prog p in
  let d_funcs = List.fold ~f:(fun so_far s -> so_far@(func_dfy gens s)) ~init:[] sl in
  let non_funcs = List.filter ~f:(fun x -> not (is_func x)) sl in
  let (Program sl) = Convertcall.prog (Program non_funcs) in
  let toplevel_stmts = List.filter ~f:is_toplevel sl in
  let d_toplevel_stmts = List.fold ~f:(fun so_far s -> so_far@(toplevel_dfy gens s)) ~init:[] toplevel_stmts in
  let non_toplevel_stmts = List.filter ~f:(fun x -> not (is_toplevel x)) sl in
  let d_non_toplevel_stmts = List.map ~f:stmt_dfy non_toplevel_stmts in
  let main = DMeth ([], (Lexing.dummy_pos, Some "Main"), [], [], [], d_non_toplevel_stmts) in
  DProg ("daffodil", d_funcs@d_toplevel_stmts@[main])
