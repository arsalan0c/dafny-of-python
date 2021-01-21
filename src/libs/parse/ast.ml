open Base
open List

exception AstError of string
type sourcemap = (int, Sourcemap.segment, Int.comparator_witness) Map.t

let sm = Hashtbl.create (module Int)

let var_counter : int ref = ref 0

let rec replicate_str s n = match n with
| 0 -> ""
| n -> s ^ replicate_str s (n - 1)

let space = " "
let indent i = replicate_str space i
let[@inline] failwith msg = raise (AstError msg)

let currLine = 0
let currColumn = 0

type literal = BooleanLiteral of bool | IntegerLiteral of int | StringLiteral of string | NoneLiteral
type identifier = Identifier of Sourcemap.segment
type unaryop = Not of Sourcemap.segment | UMinus of Sourcemap.segment

type binaryop = 
  | Plus of Sourcemap.segment 
  | Minus of Sourcemap.segment
  | Times of Sourcemap.segment
  | Divide of Sourcemap.segment
  | Mod of Sourcemap.segment
  | EqEq of Sourcemap.segment
  | NEq of Sourcemap.segment
  | Lt of Sourcemap.segment
  | LEq of Sourcemap.segment
  | Gt of Sourcemap.segment
  | GEq of Sourcemap.segment
  | And of Sourcemap.segment
  | Or of Sourcemap.segment

type exp =
  | Identifier of Sourcemap.segment
  | BinaryOp of (exp * binaryop * exp)
  | UnaryOp of (unaryop * exp)
  | Literal of literal
  | Call of (exp * exp list)

type arg = Arg of exp
type spec = Spec of exp * exp
type stmt =
  | IfElse of (exp * stmt list * stmt list)
  | While of (exp * stmt list)
  | Assign of (identifier list * exp list)
  | Function of (spec * identifier * identifier list * stmt list) (* spec, name, params, body *)
  | Return of exp
  | Assert of exp
  | Break
  | Continue
  | Print of exp
  | Exp of exp

type sexp =
  | Program of stmt list

let id_str id: identifier -> string = function
  | Identifier(s) -> (indent id) ^ (Sourcemap.segment_str s)

let literal_str id = function
  | BooleanLiteral(b) -> (indent id) ^ Bool.to_string b
  | IntegerLiteral(i) -> (indent id) ^ Int.to_string i 
  | StringLiteral(s) -> (indent id) ^ s
  | NoneLiteral -> "null"

let unaryop_str = function
  | Not s -> let v = "!" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | UMinus s -> let v = "-" in (Hashtbl.set sm ~key:currLine ~data:s); v

let binaryop_str = function
  | Plus s -> let v = "+" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | Minus s -> let v = "-" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | Times s -> let v = "*" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | Divide s -> let v = "/" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | Mod s -> let v = "%" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | NEq s -> let v = "!=" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | EqEq s -> let v = "==" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | Lt s -> let v = "<" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | LEq s -> let v = "<=" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | Gt s -> let v = ">" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | GEq s -> let v = ">=" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | And s -> let v = "&&" in (Hashtbl.set sm ~key:currLine ~data:s); v
  | Or s -> let v = "||" in (Hashtbl.set sm ~key:currLine ~data:s); v
  
let rec exp_str id = function
  | Identifier(s) -> (indent id) ^ (Sourcemap.segment_str s)
  | BinaryOp(e1, op, e2) -> (indent id) ^ String.concat ~sep:" " [(exp_str 0 e1); (binaryop_str op); (exp_str 0 e2)]
  | UnaryOp(op, e) -> (indent id) ^ (unaryop_str op) ^ (exp_str 0 e)
  | Literal(l) -> (indent id) ^ literal_str 0 l
  | Call(e, el) -> (indent id) ^ (exp_str 0 e) ^ "(" ^ (String.concat ~sep:", " (map ~f:(exp_str 0) el)) ^ ")"

let spec_str id = function
  | Spec(pre, post) -> (indent (id)) ^ "requires " ^ (exp_str 0 pre) ^ "\n" ^ (indent (id)) ^ "ensures " ^ (exp_str 0 post)

let rec stmt_str id = function
  | Exp(Call(ec, el)) -> var_counter := !var_counter + 1; ((indent id) ^ "var temp" ^ (Int.to_string !var_counter) ^ " := " ^ (exp_str 0 (Call(ec, el))) ^ ";") (* Only call expressions are allowed as statements in Dafny *)
  | Exp(_) -> ""
  | Print(e) -> (indent id) ^ "print " ^ (exp_str 0 e) ^ ";"
  | Assign(il, el) -> (indent id) ^ "var " ^ (String.concat ~sep:", " (map ~f:(id_str 0) il)) ^ " := " ^ (String.concat ~sep:", " (map ~f:(exp_str 0) el)) ^ ";"
  | IfElse(e, sl1, sl2) -> (indent id) ^ "if " ^ (exp_str 0 e) ^ " {\n" ^ 
    (String.concat ~sep:("\n") (map ~f:(stmt_str (id+2)) sl1)) ^ 
    (if length sl2 > 0 then "\n" ^ indent id ^ "} else {\n" ^ (String.concat ~sep:"\n" (map ~f:(stmt_str (id+2)) sl2)) else "") ^ "\n" ^ (indent id) ^ "}"
  | Return(e) -> (indent id) ^ "return " ^ exp_str 0 e ^ ";"
  | Assert(e) -> (indent id) ^ "assert " ^ exp_str 0 e ^ ";"
  | While(e, sl) -> (indent id) ^   "while " ^ exp_str 0 e ^ " {\n" ^ 
    (String.concat ~sep:("\n") (map ~f:(stmt_str (id+2)) sl)) ^ "\n" ^ (indent id) ^ "}"
  | Function(spec, i, il, sl) -> (indent id) ^ "method" ^ (id_str 1 i) ^ "(" ^ (String.concat ~sep:", " (map ~f:(fun x -> x ^ ": int") (map ~f:(id_str 0) il))) ^ ") returns (res: int)\n" ^ (spec_str (id+2) spec) ^ "\n" ^ (indent id) ^ "{\n" ^ (String.concat ~sep:"\n" (map ~f:(stmt_str (id+2)) sl)) ^ "\n" ^ (indent id) ^ "}"
  | _ -> failwith "unsupported AST node"

let is_fn = function
  | Function(_, _, _, _) -> true
  | _ -> false

let prog_str = function
  | Program(sl) -> let non_fn_stmts = filter ~f:(fun x -> not (is_fn x)) sl in
      let fn_stmts = filter ~f:is_fn sl in
      "method Main() {\n" ^ (String.concat ~sep:"\n" (map ~f:(stmt_str 2) non_fn_stmts)) ^ "\n}\n\n" ^
      (String.concat ~sep:"\n" (map ~f:(stmt_str 0) fn_stmts))

let nearest_seg line _ = Hashtbl.fold sm [] (fun k v res ->
    (if k = line then res@[v] else res))


(* let err_line_column msg =  *)











