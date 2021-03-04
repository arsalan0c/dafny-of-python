(* 

open Base
open Sexplib.Std

open Astpy
(* open Astdfy *)

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

let curr_line : int ref = ref 0
let curr_column : int ref = ref 1

let newline = fun () -> (curr_column := 0; curr_line := !curr_line + 1; "\n")
let newline_f f = fun s -> (f s) ^ (newline ())

let newcolumn s = (curr_column := !curr_column + (String.length s); s)

let rec newline_concat lst = 
  match lst with
  | [] -> ""
  | hd::[] -> hd
  | hd::tl -> hd ^ (newline ()) ^ (newline_concat tl)

let rec newcolumn_concat sep lst = 
  match lst with
  | [] -> ""
  | hd::[] -> newcolumn hd
  | hd::tl -> (newcolumn (hd ^ sep)) ^ (newcolumn_concat sep tl)

let ident_str id : identifier -> string = function
  | s -> newcolumn ((indent id) ^ (Sourcemap.segment_value s))

let literal_str id = function
  | BooleanLiteral(b) -> newcolumn ((indent id) ^ Bool.to_string b)
  | IntegerLiteral(i) -> newcolumn ((indent id) ^ Int.to_string i)
  | StringLiteral(s) -> newcolumn ((indent id) ^ s)
  | NoneLiteral -> newcolumn "null"

let unaryop_str = function
  | Not s -> let v = "!" in add_sm !curr_line !curr_column s; (newcolumn v)
  | UMinus s -> let v = "-" in add_sm !curr_line !curr_column s; (newcolumn v)

let binaryop_str = function
  | Plus s -> let v = "+" in add_sm !curr_line !curr_column s; (newcolumn v)
  | Minus s -> let v = "-" in add_sm !curr_line !curr_column s; (newcolumn v)
  | Times s -> let v = "*" in add_sm !curr_line !curr_column s; (newcolumn v)
  | Divide s -> let v = "/" in add_sm !curr_line !curr_column s; (newcolumn v)
  | Mod s -> let v = "%" in add_sm !curr_line !curr_column s; (newcolumn v)
  | NEq s -> let v = "!=" in add_sm !curr_line !curr_column s; (newcolumn v)
  | EqEq s -> let v = "==" in add_sm !curr_line !curr_column s; (newcolumn v)
  | Lt s -> let v = "<" in add_sm !curr_line !curr_column s; (newcolumn v)
  | LEq s -> let v = "<=" in add_sm !curr_line !curr_column s; (newcolumn v)
  | Gt s -> let v = ">" in add_sm !curr_line !curr_column s; (newcolumn v)
  | GEq s -> let v = ">=" in add_sm !curr_line !curr_column s; (newcolumn v)
  | And s -> let v = "&&" in add_sm !curr_line !curr_column s; (newcolumn v)
  | Or s -> let v = "||" in add_sm !curr_line !curr_column s; (newcolumn v)
  
let rec exp_str id = function
  | Identifier s -> add_sm !curr_line !curr_column s; newcolumn ((indent id) ^ Sourcemap.segment_value s);
  | BinaryOp(e1, op, e2) -> (newcolumn (indent id)) ^ newcolumn_concat " " [(exp_str 0 e1); (binaryop_str op); (exp_str 0 e2)]
  | UnaryOp(op, e) -> (newcolumn (indent id)) ^ (unaryop_str op) ^ (exp_str 0 e)
  | Literal l -> (newcolumn (indent id)) ^ literal_str 0 l
  | Call(e, el) -> (newcolumn (indent id)) ^ (ident_str 0 e) ^ (newcolumn "(") ^ (newcolumn_concat ", " (List.map ~f:(exp_str 0) el)) ^ (newcolumn ")")

let spec_str id = function
  | Spec(pre, post) -> (newcolumn ((indent id) ^ "requires ")) ^ (exp_str 0 pre) ^ (newline ()) ^ (newcolumn ((indent id) ^ "ensures ")) ^ (exp_str 0 post)
  | _ -> "" (* TODO: *)

let pytype_str id = function 
  | Type(t) -> let dtype = begin
    match Sourcemap.segment_value t with 
    | "int" -> "int"
    | "float" -> "float"
    | "bool" -> "bool"
    | "str" -> "string"
    | "list" -> "seq"
    | t -> t
    end in
    newcolumn ((indent id) ^ dtype)

let param_str id = function
  | Param(i, t) -> (newcolumn (indent id)) ^ (ident_str 0 i) ^ (newcolumn ": ") ^ (pytype_str 0 t)

let stmt_str id = function
  (* | Exp(Call(ec, el)) -> var_counter := !var_counter + 1; (newcolumn ((indent id) ^ "var temp" ^ (Int.to_string !var_counter) ^ " := ")) ^ (exp_str 0 (Call(ec, el))) ^ (newcolumn ";") (* Only call expressions are allowed as statements in Dafny *)
  | Exp(_) -> ""
  | Print(e) -> (newcolumn ((indent id) ^ "print ")) ^ (exp_str 0 e) ^ (newcolumn ";") *)
  | Assign(il, el) -> (newcolumn ((indent id) ^ "var ")) ^ (newcolumn_concat ", " (List.map ~f:(ident_str 0) il)) ^ (newcolumn " := ") ^ (newcolumn_concat ", " (List.map ~f:(exp_str 0) el)) ^ (newcolumn ";")
  (* | IfElse(e, sl1, sl2) -> (newcolumn ((indent id) ^ "if ")) ^ (exp_str 0 e) ^ (newcolumn " {") ^ (newline ()) ^ 
    (String.concat (List.map ~f:(newline_f (stmt_str (id+2))) sl1)) ^ 
    (if List.length sl2 > 0 then (newline ()) ^ (newcolumn (indent id ^  "} else {")) ^ (newline ()) ^ (String.concat (List.map ~f:(newline_f (stmt_str (id+2))) sl2)) else "") ^ (newline ()) ^ (newcolumn ((indent id) ^ "}"))
  | Return(e) -> (newcolumn ((indent id) ^ "return ")) ^ exp_str 0 e ^ (newcolumn ";")
  | Assert(e) -> (newcolumn ((indent id) ^ "assert ")) ^ exp_str 0 e ^ (newcolumn ";")
  | While(e, sl) -> (newcolumn ((indent id) ^   "while ")) ^ exp_str 0 e ^ (newcolumn " {")  ^ (newline ()) ^ 
    (String.concat (List.map ~f:(newline_f (stmt_str (id+2))) sl)) ^ (newline ()) ^ (indent id) ^ (newcolumn "}")
  | Function(spec, i, pl, t, sl) -> (newcolumn ((indent id) ^ "method" ^ (ident_str 1 i) ^ "(")) ^ 
    (newcolumn_concat ", " (List.map ~f:(fun x -> x) (List.map ~f:(param_str 0) pl))) ^ 
    (newcolumn ") returns (res: " ^ (pytype_str 0 t) ^ ")") ^ (newline ()) ^ (spec_str (id+2) spec) ^ 
    (newline ()) ^ (indent id) ^ (newcolumn "{") ^ (newline ()) ^ (String.concat (List.map ~f:(newline_f (stmt_str (id+2))) sl)) ^ 
    (newline ()) ^ (newcolumn ((indent id) ^ "}")) *)
  | _ -> failwith "unsupported AST node"

let is_fn = function
  | Function(_, _, _, _, _) -> true
  | _ -> false

let prog_str = function
  | Program(sl) -> let non_fn_stmts = List.filter ~f:(fun x -> not (is_fn x)) sl in
      let fn_stmts = List.filter ~f:is_fn sl in
      (newcolumn "method Main() {") ^ (newline ()) ^ (String.concat (List.map ~f:(newline_f (stmt_str 2)) non_fn_stmts)) ^ (newline ()) ^ (newcolumn "}") ^ (newline ()) ^ (newline ()) ^
      (String.concat (List.map ~f:(newline_f (stmt_str 0)) fn_stmts))

(* returns the corresponding python segment of the nearest dafny segment *)
let extr lst = match lst with
  | Some el -> el
  | None -> []

let rec nearest_seg_helper sm line column nearest = 
  match List.hd sm with
  | Some mapping -> 
    let ldiff = Int.abs ((fst (fst mapping)) - line) in 
    let so_far = fst (fst nearest) in
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

*)