open Base
open Astpy

let printf = Stdlib.Printf.printf
let var_num : int ref = ref 0
let list_constructor = "newList"

(* 
1. converts lists
2. converts subscripts
3. converts length
*)

let rec exp_lst = function
  | Literal l -> ([], Literal l)
  | Identifier ident -> ([], Identifier ident)
  | Dot (e, ident) -> let al, n_e = exp_lst e in (al, Dot (n_e, ident))
  | BinaryOp (e1, op, e2) -> 
    let al1, n_e1 = exp_lst e1 in
    let al2, n_e2 = exp_lst e2 in
    (al1@al2, BinaryOp (n_e1, op, n_e2))
  | UnaryOp (op, e) -> let al, n_e = exp_lst e in (al, UnaryOp(op, n_e))
  | Call (e, el) ->
    let als_nes = List.map ~f:exp_lst el in
    let n_el = List.fold als_nes ~f:(fun so_far (_, n_e) -> so_far@[n_e]) ~init:[] in
    let als = List.fold als_nes ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    let al, n_e = exp_lst e in
    (als@al, Call (n_e, n_el))
  | Lst el -> 
    let als_nes = List.map ~f:exp_lst el in
    let n_el = List.fold als_nes ~f:(fun so_far (_, n_e) -> so_far@[n_e]) ~init:[] in
    let als = List.fold als_nes ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    let name = var_num := !var_num + 1; "templist_" ^ (Int.to_string !var_num) in
    let n_ident = (Sourcemap.default_pos, Some name) in
    let new_list_call = Call (Identifier (Sourcemap.default_pos, Some list_constructor), [Lst n_el]) in
    (als@[Assign (Typ (NonTyp Sourcemap.default_segment), [Identifier n_ident], [new_list_call])], Identifier n_ident)
  | Tuple el -> 
    let als_nes = List.map ~f:exp_lst el in
    List.fold als_nes ~f:(
      fun (al1, lel) (al2, e) -> begin
        match lel with
        | Tuple el -> (al1@al2, Tuple (el@[e]))
        | _ -> (al1, lel)
        end
    )  ~init:([], Tuple [])
  | Len (s, e) -> 
    let al, n_e = exp_lst e in
    (al, Call (Dot (n_e, (fst s, Some "len")), []))
  | Old (s, e) -> let al, n_e = exp_lst e in (al, Old (s, n_e))
  | Forall (sl, e) -> let al, n_e = exp_lst e in (al, Forall (sl, n_e))
  | Exists (sl, e) -> let al, n_e = exp_lst e in (al, Exists (sl, n_e))
  | Subscript (e1, e2) -> 
    let al1, n_e1 = exp_lst e1 in
    let al2, n_e2 = exp_lst e2 in
    let n_e = begin match n_e2 with
      | Slice (Some _, Some _) -> failwith ""
      | Slice (Some _, None) -> failwith ""
      | Slice (None, Some _) -> failwith ""
      | Slice (None, None) -> failwith ""
      | Index i -> Call (
        Dot (
          n_e1,
          (Sourcemap.default_pos, Some "atIndex")
        ), 
        [i])
      | _ -> failwith "Second argument of subscript must be a slice or index"
    end in
    (al1@al2, n_e)
  | Index e -> let al, n_e = exp_lst e in (al, Index n_e)
  | Slice (e1, e2) -> begin
    match e1, e2 with
    | Some r1, Some r2 -> 
      let al1, n_r1 = exp_lst r1 in
      let al2, n_r2 = exp_lst r2 in
      (al1@al2, Slice (Some n_r1, Some n_r2))
    | Some r1, None ->
      let al1, n_r1 = exp_lst r1 in
      (al1, Slice (Some n_r1, None))
    | None, Some r2 -> 
      let al2, n_r2 = exp_lst r2 in
      (al2, Slice (None, Some n_r2))
    | None, None -> ([], Slice (None, None))
    end 
  | IfElseExp (e1, c, e2) -> 
    let al1, n_e1 = exp_lst e1 in
    let al2, n_c = exp_lst c in
    let al3, n_e2 = exp_lst e2 in
    (al1@al2@al3, IfElseExp (n_e1, n_c, n_e2))
  | e -> ([], e)   

let spec_lst = function
  | Pre e -> let al, n_e = exp_lst e in (al, Pre n_e)
  | Post e -> let al, n_e = exp_lst e in (al, Post n_e)
  | Invariant e -> let al, n_e = exp_lst e in (al, Invariant n_e)
  | Decreases e -> let al, n_e = exp_lst e in (al, Decreases n_e)
  | Reads e -> let al, n_e = exp_lst e in (al, Reads n_e)

let rec stmt_lst s = 
  match s with
  | Pass -> [s]
  | Break -> [s]
  | Continue -> [s]
  | Exp e -> let al, n_e = exp_lst e in al@[Exp n_e]
  | Assign (t, il, el) -> 
    let als_nes = List.map ~f:exp_lst el in
    let n_el = List.fold als_nes ~f:(fun so_far (_, n_e) -> so_far@[n_e]) ~init:[] in
    let als = List.fold als_nes ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    als@[Assign (t, il, n_el)]
  | IfElse (e, sl1, esl, sl3) -> 
    let al, n_e = exp_lst e in
    let n_sl1 = List.fold sl1 ~f:(fun so_far s -> so_far@(stmt_lst s)) ~init:[] in
    let res = List.map esl ~f:(fun (e, s) -> (exp_lst e, List.fold s ~f:(fun so_far s -> so_far@(stmt_lst s)) ~init:[])) in (* fold here to flatten list *)
    let als = List.fold res ~f:(fun so_far ((al,_), _) -> so_far@al) ~init:[] in
    let n_esl = List.fold res ~f:(fun so_far ((_, n_e), s) -> so_far@[(n_e, s)]) ~init:[] in
    let n_sl3 = List.fold sl3 ~f:(fun so_far s -> so_far@(stmt_lst s)) ~init:[] in
    al@als@[IfElse (n_e, n_sl1, n_esl, n_sl3)]
  | Return e -> 
    let al, n_e = exp_lst e in
    al@[Return n_e]
  | Assert e -> let al, n_e = exp_lst e in al@[Assert n_e]
  | While (specl, e, sl) ->
    let al, n_e = exp_lst e in
    let als_nspecl = List.map specl ~f:spec_lst in
    let n_specl = List.fold als_nspecl ~f:(fun so_far (_, n_spec) -> so_far@[n_spec]) ~init:[] in
    let als = List.fold als_nspecl ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    let n_sl = List.fold sl ~f:(fun so_far s -> so_far@(stmt_lst s)) ~init:[] in
    let aug_n_sl = n_sl@als in
    al@als@[While (n_specl, n_e, aug_n_sl)]
  | Function (specl, i, pl, t, sl) ->
    let als_nspecl = List.map specl ~f:spec_lst in
    let n_specl = List.fold als_nspecl ~f:(fun so_far (_, n_spec) -> so_far@[n_spec]) ~init:[] in
    let als = List.fold als_nspecl ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    let n_sl = List.fold sl ~f:(fun so_far s -> so_far@(stmt_lst s)) ~init:[] in
    als@[Function (n_specl, i, pl, t, n_sl)]
  | For _ -> [s]

let prog = function 
  | Program sl -> Program (List.fold sl ~f:(fun so_far s -> so_far@(stmt_lst s)) ~init:[])
