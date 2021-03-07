open Base
open Sourcemap
open Sexplib.Std

let printf = Stdlib.Printf.printf

type pos = Pos of int * int
[@@deriving sexp]
type sourcemap = (pos * Sourcemap.segment) list ref
[@@deriving sexp]
let sm = ref []
let add_sm k v s = sm := ((k, v), s)::!sm 
let var_counter : int ref = ref 0
let rec replicate_str s n = match n with
  | 0 -> ""
  | n -> s ^ replicate_str s (n - 1)

let space = " "
let indent i = replicate_str space i
let curr_line : int ref = ref 1
let curr_column : int ref = ref 1
let newline = fun () -> (curr_column := 0; curr_line := !curr_line + 1; "\n")
let newline_f f = fun s -> (f s) ^ (newline ())
let newcolumn s = (curr_column := !curr_column + (String.length s); s)

let rec newline_concat = function
  | [] -> ""
  | hd::[] -> hd
  | hd::tl -> hd ^ (newline ()) ^ (newline_concat tl)

let rec newcolumn_concat sep lst = 
  match lst with
  | [] -> ""
  | hd::[] -> newcolumn hd
  | hd::tl -> (newcolumn (hd ^ sep)) ^ (newcolumn_concat sep tl)

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
  (newline ()) ^
  newcolumn ((indent id) ^ s)

type dId = segment
[@@deriving sexp]

type dOp = 
  | DEq of segment
  | DNEq of segment
  | DPlus of segment
  | DMinus of segment
  | DTimes of segment
  | DDivide of segment
  | DMod of segment
  | DLt of segment
  | DLEq of segment
  | DGt of segment
  | DGEq of segment
  | DAnd of segment 
  | DOr of segment
  | DNot of segment
  | DBiImpl of segment
  | DImplies of segment
  | DExplies of segment
  [@@deriving sexp]

type dType = 
  | DInt of segment
  | DReal of segment 
  | DBool of segment  
  | DString of segment  
  | DChar of segment 
  | DSeq of segment * dType option
  | DArray of segment * dType option
  | DVoid
  | DTuple of segment * dType list
  [@@deriving sexp]

type dParam = dId * dType (* name: type *)
[@@deriving sexp]

type dExpr = 
  | DIdentifier of segment
  | DNull
  | DThis
  | DFresh
  | DOld
  | DIntLit of int
  | DRealLit of float
  | DBoolLit of bool
  | DStringLit of string
  | DBinary of dExpr * dOp * dExpr
  | DUnary of dOp * dExpr
  | DCallExpr of dId * dExpr list
  | DSeqExpr of dExpr list
  | DSubscript of dExpr * dExpr (* value, slice *)
  | DSlice of dExpr option * dExpr option (* lower, upper *)
  | DForall of dId list * dExpr
  | DExists of dId list * dExpr
  | DLen of dExpr
[@@deriving sexp]

type dSpec = 
  | DRequires of dExpr 
  | DEnsures of dExpr
  | DNone
  | DInvariant of dExpr
  | DDecreases of dExpr
  (* | DFresh of dExpr *)
  (* | DOld of dExpr *)
[@@deriving sexp]

type dStmt = 
  | DEmptyStmt
  | DAssume of dExpr
  | DAssert of dExpr
  | DAssign of dId list * dExpr list
  | DIf of dExpr * dStmt list * (dExpr * dStmt list) list * dStmt list
  | DWhile of dExpr * dSpec list * dStmt list
  | DPrint of dExpr
  | DReturn of dExpr list
  | DBreak
  | DYield
  | DCallStmt of dId * dExpr list
  | DMeth of dSpec list * dId * dParam list * dType list * dStmt list
[@@deriving sexp]

type dProgram =
  DProg of string * dStmt list
[@@deriving sexp]

let print_id id = function
  | s -> newcolumn ((indent id) ^ (Sourcemap.segment_value s))

let add_op id s v = add_sm !curr_line !curr_column s; (newcolumn ((indent id) ^ v))

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
  | (i, t) -> (newcolumn (indent id)) ^ (let idd = print_id 0 i in idd) ^ (newcolumn ":") ^ (print_type 1 t)

let rec print_exp id = function
  | DIdentifier s -> add_sm !curr_line !curr_column s; newcolumn ((indent id) ^ Sourcemap.segment_value s);
  | DBinary(e1, op, e2) -> (newcolumn (indent id)) ^ newcolumn "(" ^ String.concat ~sep:" " [(print_exp 0 e1); (print_op 0 op); (print_exp 0 e2);] ^ newcolumn ")"
  | DUnary(op, e) -> (newcolumn (indent id)) ^ newcolumn "(" ^ (print_op 0 op) ^ (print_exp 0 e) ^ newcolumn ")"
  | DIntLit l -> newcolumn ((indent id) ^ Int.to_string l)
  | DRealLit r -> newcolumn ((indent id) ^ Float.to_string r)
  | DBoolLit b -> newcolumn ((indent id) ^ Bool.to_string b)
  | DStringLit s -> newcolumn ((indent id) ^  "\"" ^ s ^ "\"")
  | DNull -> newcolumn ((indent id) ^ "null")
  (* | DThis -> newcolumn ((indent id) ^ "this")
  | DFresh -> newcolumn ((indent id) ^ "fresh")
  | DOld -> newcolumn ((indent id) ^ "old") *)
  | DCallExpr(e, el) -> (newcolumn (indent id)) ^ (print_id 0 e) ^ (newcolumn "(") ^ (String.concat ~sep:", " (List.map ~f:(print_exp 0) el)) ^ (newcolumn ")")
  | DSeqExpr el -> (newcolumn (indent id)) ^ (newcolumn "[") ^ (String.concat ~sep:", " (List.map ~f:(print_exp 0) el)) ^ (newcolumn "]")
  | DSubscript(e1, e2) -> (print_exp id e1) ^ (print_exp 0 e2)
  | DSlice(e1, e2) -> let res = begin
      match e1, e2 with
      | Some r1, Some r2 -> (print_exp 0 r1) ^ (newcolumn "..") ^ (print_exp 0 r2)
      | Some r1, None -> (print_exp 0 r1)
      | None, Some r2 -> (print_exp 0 r2)
      | None, None -> ""
    end
    in (newcolumn ((indent id) ^ "[")) ^ res ^ (newcolumn "]")
  | DForall(il, e) -> (newcolumn (indent id)) ^ (newcolumn "forall ") ^ (String.concat ~sep:", " (List.map ~f:(print_id 0) il)) ^ (newcolumn " :: ") ^ (print_exp 0 e)
  | DExists(il, e) -> (newcolumn (indent id)) ^ (newcolumn "exists") ^ (String.concat ~sep:", " (List.map ~f:(print_id 0) il)) ^ (newcolumn " :: ") ^ (print_exp 0 e)
  | DLen e -> (newcolumn (indent id)) ^ (newcolumn "|") ^ (print_exp 0 e) ^ (newcolumn "|")
  | _ -> failwith "unsupported expr node"

let print_spec id = function
  | DRequires c -> (newcolumn ((indent id) ^ "requires")) ^ (print_exp 1 c) 
  | DEnsures c ->  (newcolumn ((indent id) ^ "ensures")) ^ (print_exp 1 c)
  | DInvariant e -> (newcolumn ((indent id) ^ "invariant")) ^ (print_exp 1 e) 
  | DDecreases e -> (newcolumn ((indent id) ^ "decreases")) ^ (print_exp 1 e) 
  | DNone -> ""

let print_rets id = function
  | [] -> ""
  | DVoid::_ -> ""
  | tl -> (newcolumn ((indent id) ^ "returns (")) ^ (newcolumn_concat ", " 
      (List.map ~f:(fun x -> (get_name ()) ^ ":" ^ 
      (print_type 1 x)) tl)) ^ 
      (newcolumn ")")


let rec print_else id sl = if List.length sl <= 0 then "" else 
  (newcolumn " else {") ^ (newline ()) ^ (String.concat (List.map ~f:(newline_f (print_stmt (id+2))) sl)) ^ (newcolumn ((indent id) ^ "}"))

and print_elseif id esl = if List.length esl <= 0 then "" else 
  let res = fun (e, sl) -> (newcolumn " else if") ^ (print_exp 1 e) ^ (newcolumn " {") ^ (newline ()) ^ (String.concat (List.map ~f:(newline_f (print_stmt (id+2))) sl)) ^ (newcolumn ((indent id) ^ "}")) in
  String.concat ~sep:"" (List.map ~f:res esl)



and print_stmt id = function
  | DEmptyStmt -> ""
  | DAssume e -> (newcolumn ((indent id) ^ "assume")) ^ print_exp 1 e ^ (newcolumn ";") (* TODO: fix usage of newcolumn/newline *)
  | DAssert e -> (newcolumn ((indent id) ^ "assert")) ^ print_exp 1 e ^ (newcolumn ";")
  | DBreak ->  (newcolumn ((indent id) ^ "break;"))
  | DAssign(first::rest, el) -> let pre = if List.length (first::rest) = 0 || lookup (!curr_func) (Sourcemap.segment_value first) !vars then (indent id) 
    else ((vars := (!curr_func, Sourcemap.segment_value first)::!vars); (newcolumn ((indent id) ^ "var "))) in 
    pre ^ (newcolumn_concat ", " (List.map ~f:(print_id 0) (first::rest))) ^ (newcolumn " := ") ^ 
    (newcolumn_concat ", " (List.map ~f:(print_exp 0) el)) ^ (newcolumn ";")
  | DCallStmt(ident, el) -> (newcolumn (indent id)) ^ (print_id 0 ident) ^ (newcolumn "(") ^ (String.concat ~sep:", " (List.map ~f:(print_exp 0) el)) ^ (newcolumn ")") ^ (newcolumn ";")
  | DPrint e -> (newcolumn ((indent id) ^ "print")) ^ (print_exp 1 e) ^ (newcolumn ";")
  | DIf(e, sl1, sl2, sl3) -> (newcolumn ((indent id) ^ "if ")) ^ (print_exp 0 e) ^ (newcolumn " {") ^ (newline ()) ^ 
  (String.concat (List.map ~f:(newline_f (print_stmt (id+2))) sl1)) ^ (newcolumn (indent id ^  "}")) ^ (print_elseif id sl2) ^ (print_else id sl3)
  | DWhile(e, speclst, sl) -> (newcolumn ((indent id) ^ "while")) ^ print_exp 1 e ^ newcolumn_h id "" ^ (newline_concat (List.map ~f:(print_spec (id+2)) speclst)) ^ newcolumn_h id "{"  ^ (newline ()) ^ 
    (String.concat (List.map ~f:(newline_f (print_stmt (id+2))) sl)) ^ (indent id) ^ (newcolumn "}")
  | DReturn el -> (newcolumn ((indent id) ^ "return ")) ^ (newcolumn_concat ", " (List.map ~f:(print_exp 0) el)) ^ (newcolumn ";")
  | DMeth(speclst, i, pl, tl, sl) -> (curr_func := Sourcemap.segment_value i) ; 
    let prelude = (newcolumn ((indent id) ^ "method" ^ (print_id 1 i) ^ "(")) ^ 
    (newcolumn_concat ", " (List.map ~f:(fun x -> x) (List.map ~f:(print_param 0) pl))) ^ 
    (newcolumn ")") ^ print_rets 1 tl ^ (newline ()) ^ (newline_concat (List.map ~f:(print_spec (id+2)) speclst)) ^ 
    (newline ()) 
    in let midlude = (indent id) ^ (newcolumn "{") ^ (newcolumn_concat "\n" (List.map ~f:(print_declarations (id+2)) pl)) in 
    let endlude = (newline ()) ^ (String.concat (List.map ~f:(newline_f (print_stmt (id+2))) sl)) ^ (newcolumn ((indent id) ^ "}")) ^ (newline ()) in 
    prelude ^ midlude ^ endlude
  | _ -> failwith "unsupported AST node"

and print_declarations id = function
  | (i, _) -> print_stmt id (DAssign ([i], [DIdentifier i]))
  

let print_prog = function
  | DProg(_, sl) -> (String.concat (List.map ~f:(newline_f (print_stmt 0)) sl))








let extr lst = match lst with
  | Some el -> el
  | None -> []

  (* TODO: take columns into account *)
let rec nearest_seg_helper sm line column nearest = 
  match List.hd sm with
  | Some mapping -> 
    let ldiff = Int.abs ((fst (fst mapping)) - line) in 
    let so_far = Int.abs (fst (fst nearest) - line) in
    let rest = extr (List.tl sm) in
    if ldiff < so_far then nearest_seg_helper rest line column mapping else nearest_seg_helper rest line column nearest
  | None -> nearest

let nearest_seg sm line column = 
    printf "%d\n" line;
    let res = nearest_seg_helper sm line column ((Int.max_value, Int.max_value), Sourcemap.default_segment) in
    snd res

let print_pos p = "(" ^ (Int.to_string (fst p)) ^ ", " ^ (Int.to_string (snd p)) ^ "): "

let print_sm sm = String.concat ~sep:"\n" (List.map ~f:(fun e -> (print_pos (fst e)) ^ " " ^ (Sourcemap.print_segment (snd e))) sm)

(* let err_line_column msg =  *)

(* let printtbl sm = Hashtbl.iteri sm ~f:(fun ~key:x ~data:y -> (Stdlib.Printf.printf "%d -> %s\n" x (Sourcemap.print_segment y))) *)