open Base
open Pyparse.Astpy
open Pyparse.Sourcemap
let printf = Stdlib.Printf.printf

let counter_num : int ref = ref 0

let rec stmt_for s = 
  match s with
  | Pass -> [s]
  | Exp _ -> [s] 
  | Break -> [s]
  | Continue -> [s]
  | Assign _ -> [s]
  | Return _ -> [s]
  | Assert _ -> [s]
  | For (specl, il, e, sl) ->
    let counter_name = counter_num := !counter_num + 1; "tempfor_" ^ (Int.to_string !counter_num) in
    let counter_ident = (new_seg 0 0 (Some counter_name)) in 
    let assign_counter = Assign (None, [Identifier counter_ident], [Literal (IntLit "0")]) in
    let il_exps = List.map ~f:(fun id -> Identifier id) il in
    let declare_target_vars = Assign (None, il_exps, []) in
    let counter_limit = Len (def_seg, e) in
    let counter_limit_name = counter_num := !counter_num + 1; "tempfor_" ^ (Int.to_string !counter_num) in
    let counter_limit_ident =(new_seg 0 0 (Some counter_limit_name)) in 
    let assign_counter_limit = Assign (None, [Identifier counter_limit_ident], [counter_limit]) in
    let counter_inv1 = BinaryExp (Literal (IntLit "0"), LEq def_seg, Identifier counter_ident) in
    let counter_inv2 = BinaryExp (Identifier counter_ident, LEq def_seg, Identifier counter_limit_ident) in
    let counter_inv = Invariant (BinaryExp (counter_inv1, And def_seg, counter_inv2)) in
    let loop_cond = BinaryExp (Identifier counter_ident, Lt def_seg, Identifier counter_limit_ident) in
    let domain_counter = Index (Identifier counter_ident) in
    let domain_value = Subscript (e, domain_counter) in
    let assign_targets = Assign (None, il_exps, [domain_value]) in
    let rest = List.fold sl ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[] in
    let incr_counter = BinaryExp (Identifier counter_ident, Plus def_seg, Literal (IntLit "1")) in
    let update_counter = Assign (None, [Identifier counter_ident], [incr_counter]) in
    let n_sl = assign_targets::rest@[update_counter] in
    [assign_counter; assign_counter_limit; declare_target_vars; While ((counter_inv::specl), loop_cond, n_sl)]
  | IfElse (e, sl1, esl, sl2) ->
    let n_sl1 = List.fold sl1 ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[] in
    let n_esl = List.map esl ~f:(fun (e, s) -> (e, 
      List.fold s ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[])
    ) in
    let n_sl2 = List.fold sl2 ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[] in
    [IfElse (e, n_sl1, n_esl, n_sl2)]
  | While (specl, e, sl) ->
    let n_sl = List.fold sl ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[] in
    [While (specl, e, n_sl)]
  | Function (specl, i, pl, t, sl) ->
    let n_sl = List.fold sl ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[] in
    [Function (specl, i, pl, t, n_sl)]

let prog = function 
  | Program sl ->
    let n_sl = List.fold sl ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[] in
    Program n_sl
