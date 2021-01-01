(* open Core

segments: identifiers, operators, function calls

dafny segment -> python segment -> line, column
dafny line/col -> python segment -> line, column
*)

let printf = Stdlib.Printf.printf

type segment = Lexing.position * (string option)

let print_segment (s: segment) = 
  let p = fst s in
  let ln = Int.to_string p.pos_lnum in
  let cn = Int.to_string p.pos_cnum in
  let p_str = String.concat " " ["Line:"; ln; " Column:"; cn] in
  let ov = snd s in
  match ov with
    | Some v -> String.concat " " [p_str; " Value:"; v]
    | None -> p_str

let segment_str (s: segment) =
    match (snd s) with
      | Some v -> v
      | None -> ""
