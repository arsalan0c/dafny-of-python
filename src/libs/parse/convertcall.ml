open Base
open Astpy

let temp_source = Hashtbl.create (module String)

let printf = Stdlib.Printf.printf

let var_num : int ref = ref 0
let curr_block : stmt list ref = ref []

let update_block = fun () -> let res = !curr_block in curr_block := []; res

let olst = function
  | Some l -> l
  | None -> []

let concat_olst sl = List.concat (List.map ~f:olst sl)

let rec exp_calls = function
  | BinaryOp(e1, op, e2) -> 
    let al1, n_e1 = exp_calls e1 in
    let al2, n_e2 = exp_calls e2 in
    (al1@al2, BinaryOp(n_e1, op, n_e2))

  | UnaryOp(op, e) -> let al, n_e = exp_calls e in (al, UnaryOp(op, n_e))
  | Call (id, el) -> (* handle recursive case *)
    let als_nes = List.map ~f:exp_calls el in
    let n_el = List.fold als_nes ~f:(fun so_far (_, n_e) -> so_far@[n_e]) ~init:[] in
    let als = List.fold als_nes ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    let name = var_num := !var_num + 1; "temp_call" ^ (Int.to_string !var_num) in
    let _ = (match snd id with
    | Some v -> Hashtbl.add temp_source ~key:name ~data:v
    | None -> Hashtbl.add temp_source ~key:name ~data:name
    ) in
    let pos = fst id in 
    let n_id = (pos, Some name) in
    printf "%s\n" (Sourcemap.print_segment id);
    (als@[Assign ([n_id], [Call (id, n_el)])], Identifier n_id)

  | Lst el -> 
    let als_nes = List.map ~f:exp_calls el in
    List.fold als_nes ~f:(
      fun (al1, lel) (al2, e) -> begin
        match lel with
        | Lst el -> (al1@al2, Lst (el@[e]))
        | _ -> (al1, lel)
        end
    )  ~init:([], Lst [])

  (* | Subscript(e1, e2) -> DSubscript (exp_dfy e1, exp_dfy e2)
  | Slice(e1, e2) -> begin
    match e1, e2 with
    | Some r1, Some r2 -> DSlice(Some (exp_dfy r1), Some (exp_dfy r2))
    | Some r1, None -> DSlice(Some (exp_dfy r1), None)
    | None, Some r2 -> DSlice(None, Some (exp_dfy r2))
    | None, None -> DSlice (None, None)
    end *)
  (* | Forall(s, e) -> DForall(s, exp_dfy e)
  | Exists(s, e) -> DExists(s, exp_dfy e) *)
  | Len e -> let al, n_e = exp_calls e in (al, Len n_e)
  | e -> ([], e)   

let rec stmt_calls s = 
  match s with
  | Pass -> [s]
  | Exp _ -> [s] 
  | Break -> [s]
  | Continue -> [s]
  | Assign(il, el) -> 
    let als_nes = List.map ~f:exp_calls el in
    let n_el = List.fold als_nes ~f:(fun so_far (_, n_e) -> so_far@[n_e]) ~init:[] in
    let als = List.fold als_nes ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    als@[Assign(il, n_el)]
  | IfElse(e, sl1, esl, sl3) -> 
    let al, n_e = exp_calls e in
    let n_sl1 = List.fold sl1 ~f:(fun so_far ls -> so_far@(stmt_calls ls)) ~init:[] in
    let res = List.map esl ~f:(fun (e,ls) -> (exp_calls e, List.fold ls ~f:(fun so_far s -> so_far@(stmt_calls s)) ~init:[])) in (* fold here to flatten list *)
    let als = List.fold res ~f:(fun so_far ((al,_), _) -> so_far@al) ~init:[] in
    let n_esl = List.fold res ~f:(fun so_far ((_, n_e), ls) -> so_far@[(n_e, ls)]) ~init:[] in
    let n_sl3 = List.fold sl3 ~f:(fun so_far ls -> so_far@(stmt_calls ls)) ~init:[] in
    al@als@[IfElse(n_e, n_sl1, n_esl, n_sl3)]
  | Return el -> 
    let als_nes = List.map ~f:exp_calls el in
    let n_el = List.fold als_nes ~f:(fun so_far (_, n_e) -> so_far@[n_e]) ~init:[] in
    let als = List.fold als_nes ~f:(fun so_far (al, _) -> so_far@al) ~init:[] in
    als@[Return n_el]
  | Assert e -> let al, n_e = exp_calls e in al@[Assert n_e]
  | While(e, speclst, sl) -> (* TODO: handle spec *)
    let n_sl = List.fold sl ~f:(fun so_far ls -> so_far@(stmt_calls ls)) ~init:[] in
    let al, n_e = exp_calls e in
    al@[While(n_e, speclst, n_sl)]
  | Function(speclst, i, pl, t, sl) ->
    let n_sl = List.fold sl ~f:(fun so_far ls -> so_far@(stmt_calls ls)) ~init:[] in
    [Function(speclst, i, pl, t, n_sl)]

let prog = function 
  | Program sl -> Program (List.fold sl ~f:(fun so_far s -> so_far@(stmt_calls s)) ~init:[])




  (* let r = List.map ~f:(calls_to_assign e) (exp_calls e) in r@[s] *)
  (* let r = concat_olst (List.map ~f:exp_calls el) in Some r *)