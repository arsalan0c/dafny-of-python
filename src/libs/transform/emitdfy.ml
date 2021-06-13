open Base

open Astdfy
open Pyparse.Sourcemap

let printf = Stdlib.Printf.printf

type line = int
[@@deriving sexp]
type column = int
[@@deriving sexp]

type sourcemap = ((line * column) * segment) list ref
[@@deriving sexp]

let sm: sourcemap = ref []
let add_sm k v s = sm := ((k, v), s)::!sm
let rec replicate_str s n = match n with
  | 0 -> ""
  | n -> let rest = replicate_str s (n - 1) in
    String.concat [s; rest]

let space = " "
let indent i = replicate_str space i
let curr_line : int ref = ref 1
let curr_column : int ref = ref 1
let newline = fun () -> (curr_column := 1; curr_line := !curr_line + 1; "\n")
let newline_f f = fun s -> let nf = (f s) in let nl = newline () in String.concat [nf; nl]
let newcolumn s = (curr_column := !curr_column + (String.length s); s)

let rec newline_concat f = function
  | [] -> ""
  | hd::[] -> f hd
  | hd::tl -> let fhd = f hd in
    let n = newline () in 
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

let ret_param_name = fun () -> "res" 

let curr_func : string ref = ref ""

type declarations = (string * string) list ref
let vars: declarations = ref []
let add_vars vl = List.iter vl ~f:(fun v -> vars := (!curr_func, seg_val v)::!vars)

let rec lookup fn v = function
  | [] -> false
  | (f2, v2)::_ when (String.equal fn f2) && (String.equal v v2) -> true
  | _::tl -> lookup fn v tl


let newcolumn_h id s = 
  let nl = newline () in 
  let n = newcolumn (indent id) in 
  String.concat [nl; n; s]

(* if this is a temporary name, retrieve its name in the original source to store in the sourcemap *)
let source_from_temp name =
  match Base.Hashtbl.find Convertcall.temp_source name with 
  | Some v -> v
  | None -> begin 
    match Base.Hashtbl.find Convertcall.temp_source name with 
    | Some v -> v
    | None -> name
  end

let print_ident id seg =
  let n = newcolumn (indent id) in 
  let s = seg_val seg in
  let source_name = source_from_temp s in
  let n_seg = (fst seg, Some source_name) in
  add_sm !curr_line !curr_column n_seg;
  let ps = newcolumn s in
  String.concat [n; ps]

let add_op id seg v = 
  let n = newcolumn (indent id) in
  add_sm !curr_line !curr_column seg;
  let pv = newcolumn v in
  String.concat [n; pv]

let print_op id = function
  | DNotIn s -> add_op id s "!in"
  | DIn s -> add_op id s "in"
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
  | DNot s -> add_op id s "!"
  | DBiImpl s -> add_op id s "<==>"
  | DImplies s -> add_op id s "==>"
  | DExplies s -> add_op id s "<=="

let print_type id t = 
  let rec get_v t = match t with
    | DIdentTyp (s, Some t) -> (seg_val s) ^ "<" ^ (get_v t) ^ ">"
    | DIdentTyp (s, None) -> seg_val s
    | DInt _ -> "int"
    | DReal _ -> "real"
    | DBool _ -> "bool"
    | DString _ -> "string"
    | DChar _ -> "char"
    | DSeq (_, t) -> "seq<" ^ (get_v t) ^ ">"
    | DSet (_, t) -> "set<" ^ (get_v t) ^ ">"
    | DMap (_, t1, t2) -> "map<" ^ (get_v t1) ^ ", " ^ (get_v t2) ^ ">"
    | DTuple (_, tl) -> "(" ^ (String.concat ~sep:", " (List.map ~f:get_v tl)) ^ ")"
    | DFunTyp (_, tl, t) -> "(" ^ (String.concat ~sep:", " (List.map ~f:get_v tl)) ^ ") -> " ^ (get_v t)
    | _ -> ""
  in   
  let get_s t = 
    match t with
    | DIdentTyp (s, _) -> s
    | DInt s -> s
    | DReal s -> s
    | DBool s -> s
    | DString s -> s
    | DChar s -> s
    | DSeq (s, _) -> s
    | DSet (s, _) -> s
    | DMap (s, _,  _) -> s
    | DTuple (s, _) -> s 
    | DFunTyp (s, _, _) -> s
    | _ -> def_seg
  in add_op id (get_s t) (get_v t)

(* (vars := (!curr_func, idd)::!vars); *)
let print_param id = function
  | (i, t) -> 
    let n = newcolumn (indent id) in 
    let idd = print_ident 0 i in 
    let pt = match t with 
    | DVoid -> "" 
    | _ -> let c = newcolumn ":" in let pt = print_type 1 t in String.concat [c; pt] in
    String.concat [n; idd; pt]

let rec print_exp id = function
  | DIdentifier s -> let n = newcolumn (indent id) in 
    let pid = print_ident 0 s in
    String.concat [n; pid]
  | DDot (e, ident) -> let n = newcolumn (indent id) in 
    let pe = print_exp 0 e in 
    let dot = newcolumn "." in
    let pid = print_ident 0 ident in
    String.concat [n; pe; dot; pid]
  | DBinary (e1, op, e2) -> let n = newcolumn (indent id) in 
    let ob = newcolumn "(" in 
    let pe1 = print_exp 0 e1 in
    let ps1 = newcolumn " " in
    let pop = print_op 0 op in
    let ps2 = newcolumn " " in 
    let pe2 = print_exp 0 e2 in
    let cb = newcolumn ")" in
    String.concat [n; ob; pe1; ps1; pop; ps2; pe2; cb]
  | DUnary (op, e) -> let n = newcolumn (indent id) in 
    let ob = newcolumn "(" in 
    let pop = (print_op 0 op) in 
    let pe = print_exp 0 e in 
    let cb = newcolumn ")" in
    String.concat [n; ob; pop; pe; cb]
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
    let pn = newcolumn "null" in
    String.concat [n; pn]
  | DEmptyExpr -> newcolumn (indent id)
  | DThis -> let n = newcolumn (indent id) in 
    let pt = newcolumn "this" in
    String.concat [n; pt]
  | DCallExpr (e, el) -> let n = newcolumn (indent id) in 
    let pe = print_exp 0 e in 
    let ob = newcolumn "(" in 
    let pel = newcolumn_concat (print_exp 0) ", " el in 
    let cb = newcolumn ")" in
    String.concat [n; pe; ob; pel; cb]
  | DSeqExpr el -> let n = newcolumn (indent id) in 
    let ob = (newcolumn "[") in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in 
    let cb = (newcolumn "]") in 
    String.concat [n; ob; pel; cb]
  | DArrayExpr el -> let n = newcolumn (indent id) in 
    let ob = (newcolumn "[") in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in 
    let cb = (newcolumn "]") in 
    String.concat [n; ob; pel; cb] 
  | DSetExpr el -> let n = newcolumn (indent id) in 
    let ob = (newcolumn "{") in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in 
    let cb = (newcolumn "}") in 
    String.concat [n; ob; pel; cb]
  | DMapExpr eel -> let n = newcolumn (indent id) in
    let m = newcolumn "map[" in
    let peel = newcolumn_concat (
      fun (k,v) -> 
        let pk = print_exp 0 k in 
        let c = newcolumn " := " in 
        let pv = print_exp 0 v in 
        String.concat [pk; c; pv]
      ) ", " eel in 
    let cb = (newcolumn "]") in 
    String.concat [n; m; peel; cb]
  | DSubscript (e1, e2) -> let n = newcolumn (indent id) in
    let pe1 = print_exp id e1 in 
    let pe2 = print_exp 0 e2 in
    String.concat [n; pe1; pe2]
  | DIndex e -> let n = newcolumn (indent id) in
    let pe = print_exp id e in 
    String.concat [n; pe]
  | DSlice (e1, e2) ->
    let n = newcolumn (indent id) in 
    let ob = (newcolumn "[") in
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
    let cb =  (newcolumn "]") in
    String.concat [n; ob; res; cb]
  | DForall (il, e) -> let n = newcolumn (indent id) in 
    let f = (newcolumn "forall ") in 
    let pil = (newcolumn_concat (print_ident 0) ", " il) in 
    let pd = (newcolumn " :: ") in
    let pe = (print_exp 0 e) in
    String.concat [n; f; pil; pd; pe]
  | DExists (il, e) -> let n = newcolumn (indent id) in
    let ex = (newcolumn "exists") in 
    let pil = (newcolumn_concat (print_ident 0) ", " il) in
    let pc = (newcolumn " :: ") in 
    let pe = (print_exp 0 e) in
    String.concat [n; ex; pil; pc; pe]
  | DLen (_, e) -> let n = newcolumn (indent id) in 
    let ob = newcolumn "|" in 
    let pe = print_exp 0 e in
    let cb = newcolumn "|" in
    String.concat [n; ob; pe; cb]
  | DOld (_, e) -> let n = newcolumn (indent id) in 
    let old = newcolumn "old(" in 
    let pe = print_exp 0 e in
    let cb = newcolumn ")" in
    String.concat [n; old; pe; cb]
  | DFresh (_, e) -> let n = newcolumn (indent id) in 
    let fresh = newcolumn "fresh(" in 
    let pe = print_exp 0 e in
    let cb = newcolumn ")" in
    String.concat [n; fresh; pe; cb]
  | DLambda (fl, sl, e) -> let n = newcolumn (indent id) in
    let ob = newcolumn "(" in
    let pfl = newcolumn_concat (print_param 0) ", " fl in
    let cb = newcolumn ")" in
    let psl = newcolumn_concat (print_spec 0) ", " sl in
    let op = newcolumn " =>" in
    let pe = print_exp 1 e in
    String.concat [n; ob; pfl; cb; psl; op; pe]
  | DIfElseExpr (c, e1, e2) -> let n = newcolumn (indent id) in
    let i = newcolumn "if" in
    let pc = print_exp 1 c in
    let t = newcolumn " then" in
    let pe1 = print_exp 1 e1 in
    let el = newcolumn " else" in
    let pe2 = print_exp 1 e2 in
    String.concat [n; i; pc; t; pe1; el; pe2]
  | DTupleExpr el -> let n = newcolumn (indent id) in
    let ob = newcolumn "(" in
    let pel = newcolumn_concat (print_exp 0) ", " el in
    let cb = newcolumn ")" in
    String.concat [n; ob; pel; cb]

and print_spec id = function
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
  | DReads e -> let n = newcolumn (indent id) in 
    let s = newcolumn "reads" in 
    let pe = (print_exp 1 e) in
    String.concat [n; s; pe]
  | DModifies e -> let n = newcolumn (indent id) in 
    let s = newcolumn "modifies" in 
    let pe = print_exp 1 e in
    String.concat [n; s; pe]

let rec print_rets id = function
  | [] -> ""
  | DVoid::_ -> ""
  | tl -> let n = newcolumn (indent id) in 
    let r = newcolumn "(" in 
    let ptl = newcolumn_concat (
        fun x -> 
          let name = newcolumn (ret_param_name ()) in 
          let ps = newcolumn ":" in 
          let pt = print_type 1 x in 
          String.concat [name; ps; pt]
      ) ", " tl in 
    let cb = (newcolumn ")") in
    String.concat[n; r; ptl; cb]

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
  | DAssign (_, [], _) -> ""
  | DAssign (None, first::rest, el) -> let n = newcolumn (indent id) in
    let exists = (lookup (!curr_func) (seg_val first) !vars) in
    let pre = if exists then "" else (add_vars (first::rest); newcolumn "var ") in
    let pil = newcolumn_concat (print_ident 0) ", " (first::rest) in
    let pt = "" in
    let prhs = match el with 
      | [] -> "" | el -> begin
        let pa = newcolumn " := " in
        let pel = newcolumn_concat (print_exp 0) ", " el in
        String.concat[pa; pel]
      end in
    let ps = newcolumn ";" in 
    String.concat [n; pre; pil; pt; prhs; ps]
  | DAssign (Some tp, il, el) -> let n = newcolumn (indent id) in
    let pre = add_vars il; newcolumn "var " in
    let pil = newcolumn_concat (print_ident 0) ", " il in
    let pt = 
      let c = newcolumn ":" in let pt = print_type 1 tp in String.concat [c; pt]
    in
    let prhs = match el with 
      | [] -> "" | el -> begin
        let pa = newcolumn " := " in
        let pel = newcolumn_concat (print_exp 0) ", " el in
        String.concat[pa; pel]
      end in
    let ps = newcolumn ";" in 
    String.concat [n; pre; pil; pt; prhs; ps]
  | DCallStmt (e, el) -> let n = newcolumn (indent id) in 
    let pident = print_exp 0 e in 
    let ob = newcolumn "(" in 
    let pel = newcolumn_concat (print_exp 0) ", " el in
    let cb = newcolumn ")" in 
    let ps = newcolumn ";" in
    String.concat [n; pident; ob; pel; cb; ps]
  | DIf (e, sl1, sl2, sl3) -> let n = newcolumn (indent id) in
    let i = newcolumn "if " in 
    let pe = print_exp 0 e in
    let ob = newcolumn " {" in 
    let nl = newline () in
    let pst = newline_concat (print_stmt (id+2)) sl1 in
    let nl2 = newline () in
    let n2 = newcolumn (indent id) in
    let cb = newcolumn "}" in 
    let pelif = if List.length sl2 = 0 then "" else begin
      let res (e, sl) = begin
        let pel = newcolumn " else if" in
        let pe = print_exp 1 e in
        let ob = newcolumn " {" in
        let nl = newline () in 
        let pst = newline_concat (print_stmt (id+2)) sl in
        let nl2 = newline () in 
        let n = newcolumn (indent id) in
        let cb = newcolumn "}" in
        String.concat [pel; pe; ob; nl; pst; nl2; n; cb]
      end in
      newcolumn_concat res "" sl2
    end in
    let pelse = if List.length sl3 = 0 then "" else begin
      let pecb = newcolumn " else {" in 
      let nl = newline () in 
      let pst = newline_concat (print_stmt (id+2)) sl3 in
      let n = newcolumn (indent id) in 
      let nl2 = newline () in
      let n2 = newcolumn (indent id) in
      let cb = newcolumn "}" in
      String.concat [pecb; nl; pst; n; nl2; n2; cb]
    end in
    String.concat [n; i; pe; ob; nl; pst; nl2; n2; cb; pelif; pelse]
  | DWhile (speclst, e, sl) -> let n = newcolumn (indent id) in
    let w = newcolumn "while" in 
    let pe = print_exp 1 e in 
    let nl = newline () in 
    let psl = newline_concat (print_spec (id+2)) speclst in
    let ob = (newcolumn_h id "{") in 
    let nl2 = newline () in
    let pst = newline_concat (print_stmt (id+2)) sl in
    let nl3 = newline () in 
    let n2 = newcolumn (indent id) in
    let cb = newcolumn "}" in
    String.concat [n; w; pe; nl; psl; ob; nl2; pst; nl3; n2; cb]
  | DReturn el -> let n = newcolumn (indent id) in
    let r = newcolumn "return " in 
    let pel = (newcolumn_concat (print_exp 0) ", " el) in 
    let ps = newcolumn ";" in
    String.concat [n; r; pel; ps]

let print_declaration id = function
  | (i, t) -> print_stmt id (DAssign (Some t, [i], [DIdentifier i]))

let print_toplevel id = function
  | DMeth (speclst, ident, gl, pl, tl, osl) -> (curr_func := seg_val ident); 
    let n = newcolumn (indent id) in 
    let m = newcolumn "method" in
    let pident = print_ident 1 ident in
    let pgl = match gl with | [] -> "" | gl -> begin
      let ob = newcolumn "<" in
      let pvs = newcolumn_concat (fun s -> s) ", " gl in
      let cb = newcolumn ">" in
      String.concat [ob; pvs; cb]
    end in
    let ob = newcolumn "(" in    
    let pp = newcolumn_concat (print_param 0) ", " pl in
    let cb = newcolumn ")" in
    let pr = begin match tl with | [] -> "" | DVoid::_ -> "" 
      | tl -> let rt = newcolumn " returns" in let pp = print_rets 1 tl in
      String.concat [rt; pp]
    end in
    let nl = newline () in
    let pspeclst = newline_concat (print_spec (id+2)) speclst in
    let nl2 = newline () in
    let n2 = newcolumn (indent id) in
    let psl = match osl with None -> "" | Some sl ->
      let ob2 = newcolumn "{" in
      let nl3 = newline () in
      let ppl = newline_concat (print_declaration (id+2)) pl in 
      let nl4 = newline () in
      let pst = newcolumn_concat (fun x -> newline_f (print_stmt (id+2)) x) "" sl in
      let n3 = newcolumn (indent id) in
      let cb2 = newcolumn "}" in
      let nl5 = newline () in 
      String.concat [ob2; nl3; ppl; nl4; pst; n3; cb2; nl5]
    in String.concat [
      n; m; pident; pgl; ob; pp; cb; pr; nl; pspeclst; nl2; n2; psl
    ]
  | DFuncMeth (speclst, ident, gl, pl, t, oe) -> (curr_func := seg_val ident);
    let n = newcolumn (indent id) in 
    let m = newcolumn "function method" in
    let pident = print_ident 1 ident in
    let pgl = match gl with | [] -> "" | gl -> begin
      let ob = newcolumn "<" in
      let pvs = newcolumn_concat (fun s -> s) ", " gl in
      let cb = newcolumn ">" in
      String.concat [ob; pvs; cb]
    end in
    let ob = newcolumn "(" in    
    let pp = newcolumn_concat (print_param 0) ", " pl in
    let cb = newcolumn ")" in
    let pr = begin
      match t with | DVoid -> "" 
      | t -> let c = newcolumn ":" in let pp = print_rets 1 [t] in
      String.concat [c; pp]
    end in
    let nl = newline () in
    let psl = newline_concat (print_spec (id+2)) speclst in
    let nl2 = newline () in
    let pe = match oe with None -> "" | Some e ->
      let n2 = newcolumn (indent id) in
      let ob2 = newcolumn "{" in
      let nl3 = newline () in
      let n3 = newcolumn (indent (id+2)) in
      let pe = print_exp 0 e in
      let nl4 = newline () in
      let n4 = newcolumn (indent id) in
      let cb2 = newcolumn "}" in 
      let nl5 = newline () in 
      String.concat [n2; ob2; nl3; n3; pe; nl4; n4; cb2; nl5]
    in 
    String.concat [n; m; pident; pgl; ob; pp; cb; pr; nl; psl; nl2; pe]

  | DTypSynonym (ident, otyp) -> let n = newcolumn (indent id) in
    let t = newcolumn "type" in
    let pident = print_ident 1 ident in
    let pet = match otyp with | None -> "" 
      | Some typ -> let eq = newcolumn " = " in let pt = print_type 0 typ in
      String.concat [eq; pt] in
    String.concat [n; t; pident; pet] 

let print_prog = function
  | DProg(_, tll) -> newcolumn_concat (fun x -> newline_f (print_toplevel 0) x) "" tll

let extr lst = match lst with
  | Some el -> el
  | None -> []

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

(* finds the nearest dafny segment, then returns its corresponding python segment *)
let nearest_seg sm line column = 
    let res = nearest_seg_helper sm line column ((Int.max_value, Int.max_value), def_seg) in
    snd res

let print_pos p = String.concat ["("; (Int.to_string (fst p)); ", "; (Int.to_string (snd p)); "): "]
let print_sourcemap sm = String.concat ~sep:"\n" (List.map ~f:(fun e -> String.concat [(print_pos (fst e)); " "; (print_seg (snd e))]) sm)
