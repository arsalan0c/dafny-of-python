(* open Core

segments: identifiers, operators, function calls

dafny segment -> python segment -> line, column
dafny line/col -> python segment -> line, column
*)
open Core

let printf = Stdlib.Printf.printf

(* type pos = {
  	pos_fname : string;
  	pos_lnum : int;
  	pos_bol : int;
  	pos_cnum : int;
} [@@deriving sexp] *)


(* 
let sexp_of_pos p = List [Atom p.pos_fname; Atom p.pos_lnum; Atom p.pos_bol; Atom p.pos_cnum]
let pos_of_sexp s = match s with
  | List l  -> 
      match l with
      | Atom v1::Atom v2::Atom v3::Atom v4 -> { pos_fname = v1, pos_lnum = v2, pos_bol = v3, pos_cnum = v3}
      | _ -> Lexing.dummy_pos
  | _ -> Lexing.dummy_pos *)

(* type pos = Lexing.position =
{ 
  pos_fname : string
; pos_lnum : int
; pos_bol : int
; pos_cnum : int
}
[@@deriving sexp] *)

type pos = Lexing.position =
{ 
  pos_fname : string [@sexp_drop_if fun _ -> true]
; pos_lnum : int [@sexp_drop_if fun _ -> true]
; pos_bol : int [@sexp_drop_if fun _ -> true]
; pos_cnum : int [@sexp_drop_if fun _ -> true]
}
[@@deriving sexp]
type segment = pos * (string option) (* turn this into a record *)
[@@deriving sexp]


(* let sexp_of_segment = (string, ) *)

let default_segment = (Lexing.dummy_pos, None)

let print_pos (p: pos) = 
  let ln = Int.to_string p.pos_lnum in
  let cn = Int.to_string (p.pos_cnum - p.pos_bol) in
  String.concat ~sep:" " ["Line:"; ln; " Column:"; cn]
  

let print_segment (s: segment) = 
  let p = fst s in
  let p_str = print_pos p in
  let ov = snd s in
  match ov with
    | Some v -> String.concat ~sep:" " [p_str; " Value:"; v]
    | None -> p_str

let segment_value (s: segment) =
    match (snd s) with
      | Some v -> v
      | None -> ""

let segment_pos (s: segment) = fst s
