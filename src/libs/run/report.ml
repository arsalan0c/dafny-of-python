open Base
open Pyparse.Sourcemap

exception ReportError of string
let[@inline] failwith msg = raise (ReportError msg)

let printf = Stdlib.Printf.printf
let prerr = Stdlib.prerr_string

let replace_num p =
  let nums = (Re2.find_all_exn (Re2.create_exn "[0-9]*") p) in
  let line_column = List.filter ~f:(fun s -> 
    let s = String.length s in if s = 0 then false else true) nums in
  let line = begin
    match (List.nth line_column 0) with
    | Some s -> Int.of_string s
    | None -> 0
    end in
  let column = begin
    match (List.nth line_column 1) with
    | Some s -> Int.of_string s
    | None -> 0
    end in
  let seg = Transform.Emitdfy.nearest_seg (!Transform.Emitdfy.sm) line column in
  let seg_str = print_seg seg in seg_str

let verification_errors out =
  try begin
    let line_rgx = Re2.create_exn "\([0-9]*,[0-9]*\).+" in
    let lines = Re2.find_all_exn line_rgx out in 
    let split_rgx = Re2.create_exn ":" in
    let split_f s = Re2.split split_rgx s in
    let split_lines = List.map ~f:split_f lines in
    let replaced_nums = List.map ~f:(fun lst -> List.mapi ~f:(fun i s -> if i = 0 then replace_num s else s) lst) split_lines in
    let errors = List.map replaced_nums ~f:(fun l -> String.concat ~sep:", " l) in
    let errors_s = String.concat ~sep:"\n" errors in
    Some errors_s
  end with
  | Re2.Exceptions.Regex_match_failed _ -> None

let prelude_verified = 42
let verification_summary out =
  try begin
    let line_rgx = Re2.create_exn ("verifier finished with [0-9]+ verified, [0-9]+ error") in (* TODO: substract prelude from verified count *)
    let line = (Re2.find_first_exn line_rgx out) ^ "(s)" in
    let num_rgx = Re2.create_exn "[0-9]+" in 
    let num_verified = Re2.find_first_exn num_rgx line in
    let num_verified_source = Int.to_string ((Int.of_string num_verified) - prelude_verified) in
    let verified_line = String.substr_replace_first line ~pattern:num_verified ~with_:num_verified_source in
    printf "%s\n" verified_line
  end with
  | Re2.Exceptions.Regex_match_failed e -> failwith ("\nUnable to obtain verification summary due to regex match fail: " ^ e ^ "\n")

let report out = begin match verification_errors out with
  | Some s -> prerr (s ^ "\n")
  | None -> ()
  end;
  verification_summary out
