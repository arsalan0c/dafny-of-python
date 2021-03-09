open Base
open Sexplib.Std

open Astdfy

let printf = Stdlib.Printf.printf

type pos = Pos of int * int (* line, column *)
[@@deriving sexp]
type sourcemap = (pos * Sourcemap.segment) list ref
[@@deriving sexp]

let sm = ref []
let add_sm k v s = sm := ((k, v), s)::!sm 
let var_counter : int ref = ref 0
let rec replicate_str s n = match n with
  | 0 -> ""
  | n -> let rest = replicate_str s (n - 1) in
    String.concat [s; rest]

let space = " "
let indent i = replicate_str space i
let curr_line : int ref = ref 1
let curr_column : int ref = ref 1
let newline = fun () -> (curr_column := 1; curr_line := !curr_line + 1; "\n")
let newline_f f = fun s -> let nf = (f s) in let nl = (newline ()) in String.concat [nf; nl]
let newcolumn s = (curr_column := !curr_column + (String.length s); s)

let rec newline_concat f = function
  | [] -> ""
  | hd::[] -> f hd
  | hd::tl -> let fhd = f hd in
    let n = (newline ()) in 
    let rest = newline_concat f tl in 
    String.concat [fhd; n; rest]

let rec newcolumn_concat f sep = function
  | [] -> ""
  | hd::[] -> f hd
  | hd::tl ->
    let fhd = f hd in 
    let n = newcolumn sep in
    let rest = newcolumn_concat f sep tl in
    String.concat [fhd; n; rest]

let get_name = fun () ->
  var_counter := !var_counter + 1;
  "res" ^ Int.to_string (!var_counter)

type declarations = (string * string) list ref
let vars: declarations = ref []

let rec lookup fn v = function
  | [] -> false
  | (f2, v2)::_ when (String.equal fn f2) && (String.equal v v2) -> true
  | _::tl -> lookup fn v tl

let curr_func : string ref = ref ""

let newcolumn_h id s = 
  let nl = newline () in 
  let n = newcolumn (indent id) in 
  String.concat [nl; n; s]

let print_id id s =
  let n = newcolumn (indent id) in 
  let s = (Sourcemap.segment_value s) in
  String.concat [n; s]

let add_op id s v = add_sm !curr_line !curr_column s;
  let n = newcolumn (indent id) in
  let pv = newcolumn v in
  String.concat [n; pv]

let print_op id = function
  | DPlus s -> add_op id s "+"
  | DMinus s -> add_op id s "-"
  | DTimes s -> add_op id s "*"
  | DDivide s -> add_op id s "/"
  | DMod s -> add_op id s "%"
  | DNEq s -> add_op id s "!="
  | DEq s -> add_op id s "=="
  | DLt s -> add_op id s "<"
  | DLEq s -> add_op id s "<="
  | DGt s -> add_op id s ">"
  | DGEq s -> add_op id s ">=" 
  | DAnd s -> add_op id s "&&" 
  | DOr s -> add_op id s "||"
  | DBiImpl s -> add_op id s "<==>"
  | DImplies s -> add_op id s "==>"
  | DExplies s -> add_op id s "<=="
  | _ -> failwith "unsupported op node"

let print_type id t = 
  let rec get_v t = match t with
    | DInt _ -> "int"
    | DReal _ -> "real"
    | DBool _ -> "bool"
    | DString _ -> "string"
    | DChar _ -> "char"
    | DSeq(_, ot) -> begin
      match ot with
      | Some t -> "seq<" ^ (get_v t) ^ ">"
      | None -> "seq"
      end
    (* | DArray s -> add_op id s "array" *)
    | DTuple(_, sl) -> "(" ^ (String.concat ~sep:", " (List.map ~f:get_v sl)) ^ ")"
    | _ -> ""
  in   
  let get_s t = 
    match t with
    | DInt s -> s
    | DReal s -> s
    | DBool s -> s
    | DString s -> s
    | DChar s -> s
    | DSeq(s, _) -> s
    | DTuple(s, _) -> s 
    (* | DArray s -> add_op id s "array" *)
    | _ -> Sourcemap.default_segment
  in add_op id (get_s t) (get_v t)

(* (vars := (!curr_func, idd)::!vars); *)
let print_param id = function
  | (i, t) -> 
    let n = (newcolumn (indent id)) in 
    let idd = print_id 0 i in 
    let c = (newcolumn ":") in 
    String.concat [n; idd; c; print_type 1 t]

let rec print_exp id = function
  | DIdentifier s -> add_sm !curr_line !curr_column s; let n = newcolumn (indent id) in
    let ps = newcolumn (Sourcemap.segment_value s) in
    String.concat [n; ps]
  | DBinary(e1, op, e2) -> let n = (newcolumn (indent id)) in 
    let pob = (newcolumn "(") in 
    let pe1 = (print_exp 0 e1) in (* TODO: this does not work *)
    let ps1 = newcolumn " " in
    let pop = (print_op 0 op) in
    let ps2 = newcolumn " " in 
    let pe2 = (print_exp 0 e2) in
    let pcb = newcolumn ")" in
    String.concat [n; pob; pe1; ps1; pop; ps2; pe2; pcb]
  | DUnary(op, e) -> let n = (newcolumn (indent id)) in 
    let pob = newcolumn "(" in 
    let pop = (print_op 0 op) in 
    let pe = print_exp 0 e in 
    let pcb = newcolumn ")" in
    String.concat [n; pob; pop; pe; pcb]
  | DIntLit i -> let n = newcolumn (indent id) in 
    let si = Int.to_string i in
    String.concat [n; si]
  | DRealLit r -> let n = newcolumn (indent id) in 
    let sr = Float.to_string r in
    String.concat [n; sr]
  | DBoolLit b -> let n = newcolumn (indent id) in 
    let sb = Bool.to_string b in
    String.concat [n; sb]
  | DStringLit s -> let n = newcolumn (indent id) in 
    let es = "\"" ^ s ^ "\"" in
    String.concat [n; es]
  | DNull -> let n = newcolumn (indent id) in 
    String.concat [n; "null"]
  (* | DThis -> newcolumn ((indent id) ^ "this")
  | DFresh -> newcolumn ((indent id) ^ "fresh")
  | DOld -> newcolumn ((indent id) ^ "old") *)
  | DCallExpr(e, el) -> let n = (newcolumn (indent id)) in 
    let pe = (print_id 0 e) in 
    let pob = (newcolumn "(") in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in 
    let pcb = (newcolumn ")") in
    String.concat [n; pe; pob; pel; pcb]
  | DSeqExpr el -> let n = (newcolumn (indent id)) in 
    let pob = (newcolumn "[") in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in 
    let pcb = (newcolumn "]") in 
    String.concat [n; pob; pel; pcb]
  | DSubscript(e1, e2) -> let n = (newcolumn (indent id)) in
    let pe1 = (print_exp id e1) in 
    let pe2 = (print_exp 0 e2) in
    String.concat [n; pe1; pe2]
  | DSlice(e1, e2) ->
    let n = newcolumn (indent id) in 
    let pob = (newcolumn "[") in
    let res = begin
      match e1, e2 with
      | Some r1, Some r2 -> let pe1 = (print_exp 0 r1) in 
        let pd = (newcolumn "..") in 
        let pe2 = (print_exp 0 r2) in
        String.concat [pe1; pd; pe2]
      | Some r1, None -> (print_exp 0 r1)
      | None, Some r2 -> (print_exp 0 r2)
      | None, None -> ""
    end
    in 
    let pcb =  (newcolumn "]") in
    String.concat [n; pob; res; pcb]
  | DForall(il, e) -> let n = newcolumn (indent id) in 
    let f = (newcolumn "forall ") in 
    let pil = (newcolumn_concat (print_id 0) ", " il) in 
    let pd = (newcolumn " :: ") in
    let pe = (print_exp 0 e) in
    String.concat [n; f; pil; pd; pe]
  | DExists(il, e) -> let n = newcolumn (indent id) in
    let ex = (newcolumn "exists") in 
    let pil = (newcolumn_concat (print_id 0) ", " il) in
    let pc = (newcolumn " :: ") in 
    let pe = (print_exp 0 e) in
    String.concat [n; ex; pil; pc; pe]
  | DLen e -> let n = newcolumn (indent id) in 
    let pob = (newcolumn "|") in 
    let pe = (print_exp 0 e) in
    let pcb = (newcolumn "|") in
    String.concat [n; pob; pe; pcb]
  | _ -> failwith "unsupported expr node"

let print_spec id = function
  | DRequires e -> let n  = newcolumn (indent id) in 
    let s = newcolumn "requires" in 
    let pe = (print_exp 1 e) in
    String.concat [n; s; pe] 
  | DEnsures e ->  let n  = newcolumn (indent id) in 
    let s = newcolumn "ensures" in 
    let pe = (print_exp 1 e) in
    String.concat [n; s; pe]
  | DInvariant e -> let n  = newcolumn (indent id) in 
    let s = newcolumn "invariant" in 
    let pe = (print_exp 1 e) in
    String.concat [n; s; pe] 
  | DDecreases e -> let n  = newcolumn (indent id) in 
    let s = newcolumn "decreases" in 
    let pe = (print_exp 1 e) in
    String.concat [n; s; pe]
  | DNone -> ""

let rec print_rets id = function
  | [] -> ""
  | DVoid::_ -> ""
  | tl -> let n = newcolumn (indent id) in 
    let r = newcolumn "returns (" in 
    let ptl = newcolumn_concat (
        fun x -> let name = get_name () in 
          let ps = newcolumn ":" in 
          let pt = print_type 1 x in 
          String.concat [name; ps; pt]
      ) ", " tl in 
    let pcb = (newcolumn ")") in
    String.concat[n; r; ptl; pcb]

and print_stmt id = function
  | DEmptyStmt -> ""
  | DAssume e -> let n = newcolumn (indent id) in 
    let a = newcolumn "assume" in 
    let pe = print_exp 1 e in 
    let ps = (newcolumn ";") in
    String.concat [n; a; pe; ps]
  | DAssert e -> let n = newcolumn (indent id) in 
    let a = newcolumn "assert" in 
    let pe = print_exp 1 e in 
    let ps = (newcolumn ";") in
    String.concat [n; a; pe; ps]
  | DBreak -> let n = newcolumn (indent id) in 
    let b = newcolumn "break" in 
    let ps = (newcolumn ";") in
    String.concat [n; b; ps]
  | DAssign(first::rest, el) -> 
    let exists = (List.length (first::rest) = 0) || (lookup (!curr_func) (Sourcemap.segment_value first) !vars) in
    let n = newcolumn (indent id) in
    let pre = if exists then "" else begin
      vars := (!curr_func, Sourcemap.segment_value first)::!vars; (* Add to variable store *)
      newcolumn "var "
    end in
    let pil = newcolumn_concat (print_id 0) ", " (first::rest) in 
    let pa = newcolumn " := " in
    let pel = newcolumn_concat (print_exp 0) ", " el in
    let ps = newcolumn ";" in 
    String.concat [n; pre; pil; pa; pel; ps]
  | DCallStmt(ident, el) -> let n = (newcolumn (indent id)) in 
    let pident = print_id 0 ident in 
    let pob = newcolumn "(" in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in
    let pcb = (newcolumn ")") in 
    let ps = (newcolumn ";") in
    String.concat [n; pident; pob; pel; pcb; ps]
  | DPrint e -> let n = newcolumn (indent id) in 
    let p = newcolumn "print" in 
    let pe = (print_exp 1 e) in 
    let ps = (newcolumn ";") in
    String.concat [n; p; pe; ps]
  | DIf(e, sl1, sl2, sl3) -> let n = newcolumn (indent id) in
    let i = newcolumn "if " in 
    let pe = print_exp 0 e in
    let pob = newcolumn " {" in 
    let nl = newline () in
    let pst = newline_concat (print_stmt (id+2)) sl1 in
    let nl2 = newline () in
    let n2 = newcolumn (indent id) in
    let pcb = newcolumn "}" in 
    let pelif = if List.length sl2 <= 0 then "" else begin
      let res (e, sl) = begin
        let pel = newcolumn " else if" in
        let pe = print_exp 1 e in
        let pob = newcolumn " {" in
        let nl = newline () in 
        let pst = newline_concat (print_stmt (id+2)) sl in
        let nl2 = newline () in 
        let n = newcolumn (indent id) in
        let pcb = newcolumn "}" in
        String.concat [pel; pe; pob; nl; pst; nl2; n; pcb]
      end in
      newcolumn_concat res "" sl2
    end in
    let pelse = if List.length sl3 <= 0 then "" else begin
      let pecb = newcolumn " else {" in 
      let nl = (newline ()) in 
      let pst = newline_concat (print_stmt (id+2)) sl3 in
      let n = newcolumn (indent id) in 
      let nl2 = newline () in
      let n2 = newcolumn (indent id) in
      let pcb = newcolumn "}" in
      String.concat [pecb; nl; pst; n; nl2; n2; pcb]
    end in
    String.concat [n; i; pe; pob; nl; pst; nl2; n2; pcb; pelif; pelse]
  | DWhile(e, speclst, sl) -> let n = newcolumn (indent id) in
    let w = newcolumn "while" in 
    let pe = print_exp 1 e in 
    let nl = (newline ()) in 
    let psl = newline_concat (print_spec (id+2)) speclst in
    let pob = (newcolumn_h id "{") in 
    let nl2 = newline () in
    let pst = newline_concat (print_stmt (id+2)) sl in
    let nl3 = newline () in 
    let n2 = newcolumn (indent id) in
    let pcb = newcolumn "}" in
    String.concat [n; w; pe; nl; psl; pob; nl2; pst; nl3; n2; pcb]
  | DReturn el -> let n = newcolumn (indent id) in
    let r = newcolumn "return " in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in 
    let ps = newcolumn ";" in
    String.concat [n; r; pel; ps]
  | DMeth(speclst, ident, pl, tl, sl) -> (curr_func := Sourcemap.segment_value ident); 
    let n = newcolumn (indent id) in 
    let m = newcolumn "method" in
    let pident = print_id 1 ident in
    let pob = newcolumn "(" in    
    let pp = newcolumn_concat (print_param 0) ", " pl in
    let pcb = newcolumn ")" in
    let pr = print_rets 1 tl in
    let nl = (newline ()) in
    let psl = newline_concat (print_spec (id+2)) speclst in
    let nl2 = (newline ()) in
    let n2 = newcolumn (indent id) in 
    let pob2 = newcolumn "{" in
    let nl3 = (newline ()) in
    let ppl = (newline_concat (print_declarations (id+2)) pl) in 
    let nl4 = (newline ()) in
    let pst = newcolumn_concat (fun x -> newline_f (print_stmt (id+2)) x) "" sl in
    let n3 = newcolumn (indent id) in
    let pcb2 = newcolumn "}" in 
    let nl5 = newline () in 
    String.concat [n; m; pident; pob; pp; pcb; pr; nl; psl; nl2; n2; pob2; nl3; ppl; nl4; pst; n3; pcb2; nl5]
  | _ -> failwith "unsupported AST node"

and print_declarations id = function
  | (i, _) -> print_stmt id (DAssign ([i], [DIdentifier i]))
  
let print_prog = function
  | DProg(_, sl) -> newcolumn_concat (fun x -> newline_f (print_stmt 0) x) "" sl

let extr lst = match lst with
  | Some el -> el
  | None -> []

(* TODO: take columns into account *)
let rec nearest_seg_helper sm line column nearest = 
  match List.hd sm with
  | Some mapping -> 
    let ldiff = Int.abs ((fst (fst mapping)) - line) in 
    let l_so_far = Int.abs ((fst (fst nearest)) - line) in
    let rest = extr (List.tl sm) in
    if ldiff < l_so_far then nearest_seg_helper rest line column mapping
    else if ldiff = l_so_far then begin
      let cdiff = Int.abs ((snd (fst mapping)) - column) in
      let c_so_far = Int.abs ((snd (fst nearest)) - column) in
      if cdiff < c_so_far then nearest_seg_helper rest line column mapping 
      else nearest_seg_helper rest line column nearest
    end 
    else nearest_seg_helper rest line column nearest
  | None -> nearest

let nearest_seg sm line column = 
    printf "%d\n" line;
    let res = nearest_seg_helper sm line column ((Int.max_value, Int.max_value), Sourcemap.default_segment) in
    snd res

let print_pos p = String.concat ["("; (Int.to_string (fst p)); ", "; (Int.to_string (snd p)); "): "]

let print_sourcemap sm = String.concat ~sep:"\n" (List.map ~f:(fun e -> String.concat [(print_pos (fst e)); " "; (Sourcemap.print_segment (snd e))]) sm)
