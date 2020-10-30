
open Pyparse.Ast
open Z3

let ctx = mk_context []
let env = Hashtbl.create 10
let printf = Stdlib.Printf.printf

let is_fn = function
  | Function(_, _, _, _) -> true
  | _ -> false

let id_z3 : identifier -> string = function
  | Identifier(ident) -> ident

let literal_z3 = function
  (* | BooleanLiteral(b) -> b *)
  | IntegerLiteral(i) -> Arithmetic.Integer.mk_numeral_i ctx i
  (* | StringLiteral(s) -> s *)
  (* | NoneLiteral -> false *)
  | _ -> Arithmetic.Integer.mk_numeral_i ctx 99  

let rec exp_z3 = function
  | Identifier(ident) -> (printf "\n%s\n" ident; Hashtbl.find env ident)
  | BinaryOp(e1, op, e2) -> begin 
    match op with
    | Plus -> Arithmetic.mk_add ctx [exp_z3 e1; exp_z3 e2]
    | Minus -> Arithmetic.mk_sub ctx [exp_z3 e1; exp_z3 e2]
    | Times -> Arithmetic.mk_mul ctx [exp_z3 e1; exp_z3 e2]
    | Divide -> Arithmetic.mk_div ctx (exp_z3 e1) (exp_z3 e2)
    | Mod -> Arithmetic.Integer.mk_mod ctx (exp_z3 e1) (exp_z3 e2)
    | NEq -> Boolean.mk_not ctx (Boolean.mk_eq ctx (exp_z3 e1) (exp_z3 e2))
    | EqEq -> Boolean.mk_eq ctx (exp_z3 e1) (exp_z3 e2)
    | Lt -> Arithmetic.mk_lt ctx (exp_z3 e1) (exp_z3 e2)
    | LEq -> Arithmetic.mk_le ctx (exp_z3 e1) (exp_z3 e2)
    | Gt -> Arithmetic.mk_gt ctx (exp_z3 e1) (exp_z3 e2)
    | GEq -> Arithmetic.mk_ge ctx (exp_z3 e1) (exp_z3 e2)
    | And -> Boolean.mk_and ctx [exp_z3 e1; exp_z3 e2]
    | Or -> Boolean.mk_or ctx [exp_z3 e1; exp_z3 e2]
    end
  | UnaryOp(op, e) -> begin
    match op with
    | Not -> Boolean.mk_not ctx (exp_z3 e) 
    | UMinus -> Arithmetic.mk_unary_minus ctx (exp_z3 e)
    end 
  | Literal(l) -> literal_z3 l
  | Call(e, _) -> Arithmetic.mk_unary_minus ctx (exp_z3 e)

let stmt_z3 post = function
  | Assign(il, el) -> begin
    let e = exp_z3 (List.nth el 0) in
    match e with
      | x -> 
        let id = id_z3 (List.nth il 0) in
        let ide = Arithmetic.Integer.mk_const_s ctx id in
        let _ = Hashtbl.add env id ide in
        let sub = Expr.substitute post [ide] [x] in
        sub
        (* let imp = Boolean.mk_implies ctx pre sub in *)
        (* let neg_imp = Boolean.mk_not ctx imp in
        let sol = Solver.mk_simple_solver ctx in
        let res = (Solver.string_of_status (Solver.check sol [neg_imp])) in
        if res = "satisfiable" then  *)
    end 
  | _ -> post
  (* | IfElse(e, sl1, sl2) ->
  | Return(e) ->
  | Assert(e) ->
  | While(e, sl) -> *)

let rec composition pre post sl = if List.length sl > 1 then composition pre (stmt_z3 post (List.hd sl)) (List.tl sl) else 
  begin
    let sub = stmt_z3 post (List.hd sl) in
    let imp = Boolean.mk_implies ctx pre sub in
    let neg_imp = Boolean.mk_not ctx imp in
    let sol = Solver.mk_simple_solver ctx in
    let res = (Solver.string_of_status (Solver.check sol [neg_imp])) in
    Some res
  end



let func_z3 = function
  | Function(Spec(pre, post), ident, il, sl) -> begin
    let f = (fun id -> let ids = (id_z3 id) in let ide = Arithmetic.Integer.mk_const_s ctx ids in Hashtbl.add env ids ide) in
      let _ = List.iter f il in
      match composition (exp_z3 pre) (exp_z3 post) (List.rev sl) with
        | Some x -> let name = (id_z3 ident) in 
          let msg = if x = "satisfiable" then "Unable to verify " else "Successfully verified " in
          msg ^ "<" ^ name ^ ">"
        | None -> ""
    end 
  | _ -> ""

let prog_z3 = function
  | Program(sl) -> String.concat "\n" (List.map func_z3 sl) 





