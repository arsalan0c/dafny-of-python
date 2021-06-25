open Base

let printf = Stdlib.Printf.printf

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

type linecol = int * int
[@@deriving sexp]
type sourcemap = (linecol * segment) list ref
[@@deriving sexp]

let def_pos = Lexing.dummy_pos
let def_seg = (def_pos, None)

let print_pos (p: pos) = 
  let ln = Int.to_string p.pos_lnum in
  let cn = Int.to_string (p.pos_cnum - p.pos_bol) in
  String.concat ~sep:" " ["Line:"; ln; " Column:"; cn]

let new_seg l c v = 
  ({pos_fname=""; pos_lnum=l; pos_bol=0; pos_cnum=c}, v)

let update_seg_val s v =
    (fst s, v)
  
let print_seg (s: segment) = 
  let p = fst s in
  let p_str = print_pos p in
  let ov = snd s in
  match ov with
    | Some v -> String.concat ~sep:" " [p_str; " Value:"; v]
    | None -> p_str

let seg_val (s: segment) =
    match (snd s) with
      | Some v -> v
      | None -> ""
  
let seg_val_compare s1 s2 =
  String.compare (seg_val s1) (seg_val s2)

let seg_pos (s: segment) = fst s
