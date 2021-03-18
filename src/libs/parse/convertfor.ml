open Base
open Astpy

let printf = Stdlib.Printf.printf

let counter_num : int ref = ref 0
(* let limit_num : int ref = ref 0 *)

let rec stmt_for s = 
  match s with
  | Pass -> [s]
  | Exp _ -> [s] 
  | Break -> [s]
  | Continue -> [s]
  | Assign _ -> [s]
  | Return _ -> [s]
  | Assert _ -> [s]
  | For (specl, el, e, sl) ->
    let counter_name = counter_num := !counter_num + 1; "tempforcounter_" ^ (Int.to_string !counter_num) in
    let counter_ident = Identifier (Sourcemap.new_segment 0 0 (Some counter_name)) in 
    let assign_counter = Assign (Typ Void, [counter_ident], [Literal (IntLit 0)]) in
    let declare_target_vars = Assign (Typ Void, el, []) in
    let counter_limit = Len (Sourcemap.default_segment, e) in
    let loop_cond = BinaryOp (counter_ident, Lt Sourcemap.default_segment, counter_limit) in
    let domain_counter = Slice (Some (counter_ident), None) in
    let domain_value = Subscript (e, domain_counter) in
    let assign_targets = Assign (Typ Void, el, [domain_value]) in
    let rest = List.fold sl ~f:(fun so_far s -> so_far@(stmt_for s)) ~init:[] in
    let incr_counter = BinaryOp (counter_ident, Plus Sourcemap.default_segment, Literal (IntLit 1)) in
    let update_counter = Assign (Typ Void, [counter_ident], [incr_counter]) in
    let n_sl = assign_targets::rest@[update_counter] in
    [assign_counter; declare_target_vars; While (specl, loop_cond, n_sl)]
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
