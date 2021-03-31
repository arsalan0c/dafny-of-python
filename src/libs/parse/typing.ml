open Base
open Astpy
open Sourcemap
open Sexplib

(* module type TYPING = sig
  type var = string
  type ctx = (var * typ option) list
  type 'a t = ctx -> ('a * ctx) option

  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'b -> ('a * 'b) option
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val synth : exp -> typ t
  val check : exp -> typ -> unit t
  val fail : 'a t
  val check_var : var -> typ -> unit t
  val lookup : var -> typ t
  val with_var : var -> 'a t -> 'a t
  val typ_eq : typ -> typ -> unit t
end *)

(* module Typing : TYPING = struct *)
type var = string
type ctx = (var * typ option) list
type 'a t = ctx -> ('a * ctx) option

let typ_print otp = match otp with | Some tp -> Sexp.to_string (sexp_of_typ tp) | None -> ""
let print (ctx: ctx) : unit =
  let f (v, otp) : string =
    "(" ^ v ^ ", " ^ typ_print otp ^ ")"
  in
  let printf = Stdlib.Printf.printf in
  printf "%s\n" ("context: [" ^ (String.concat ~sep:", " (List.map ctx ~f:f)) ^ "]")

let return x = fun ctx -> Some (x, ctx)
let (>>=) m f = fun ctx -> 
  let open Options in
  m ctx >>= fun (a, ctx') -> f a ctx'

let fail = fun _ -> None

let typ_eq tp1 tp2 = if typ_compare tp1 tp2 = 0 then return () else fail

let rec check_var (x: var) (tp: typ) : unit t = fun ctx ->
  let open Options in
  match ctx with
  | [] -> failwith "" (* out-of-scope variable reference *)
  | (y, None)::rest when String.compare x y = 0 -> return ((), (y, Some tp)::rest)
  | (y, Some _)::_ when String.compare x y = 0 -> failwith "" (* variables should be single-use *)
  | h::rest -> check_var x tp rest >>= fun ((), rest') -> 
    return ((), h::rest')
(* 
let rec synth_var (x: var) (tp: typ) : unit t = fun ctx ->
    let open Options in
    match ctx with
    | [] -> failwith "" (* out-of-scope variable reference *)
    | (y, None)::rest when String.compare x y = 0 -> return ((), (y, Some tp)::rest)
    | (y, Some _)::_ when String.compare x y = 0 -> failwith "" (* variables should be single-use *)
    | h::rest -> check_var x tp rest >>= fun ((), rest') -> 
      return ((), h::rest') *)

let lookup x = fun (ctx: ctx) ->
  match List.Assoc.find ctx ~equal:(fun x y -> String.compare x y = 0) x with
  | None -> Options.fail
  | Some None -> Options.fail
  | Some (Some tp) -> Options.return (tp, ctx)

(* when we want to run m with a variable, m gives the variable its type *)
let with_var (type a) (x: var) (m: a t) : a t = fun ctx -> 
  let open Options in
  m ((x, None)::ctx) >>= function
  | (r, (y, Some _)::ctx') when String.compare x y = 0 -> return (r, ctx') 
  | (_, (y, None)::_) when String.compare x y = 0 -> failwith "" (* type of variable should have been set *)
  | _ -> assert false

(* use - return type, context *)
let rec synth_exp e = match e with
  | Identifier ident -> lookup (seg_val ident)
  | Typ tp -> return tp
  | Literal BoolLit _ -> return (TBool def_seg)
  | Literal IntLit _ -> return (TInt def_seg)
  | Literal FloatLit _ -> return (TFloat def_seg)
  | Literal StringLit _ -> return (TStr def_seg)
  | Literal NoneLit -> return (TNone def_seg)
  | UnaryExp (Not _, e) -> let tp = TBool def_seg in check_exp e tp >>= fun () -> return tp
  | UnaryExp (UMinus _, e) -> let tp = TFloat def_seg in check_exp e tp >>= fun () -> return tp
  | BinaryExp (e1, op, e2) -> begin
    match op with
    (* | Plus _ -> check e1 tp >>= check e2 tp2 with | Some tp *)
    | Lt _ -> let tp = TFloat def_seg in check_exp e1 tp >>= fun () -> check_exp e2 tp >>= fun () -> return tp
    (* | And _ -> let tp = TBool def_seg in check_exp e1 tp >>= fun () -> check_exp e2 tp >>= fun () -> return tp
    | Or _ -> let tp = TBool def_seg in check_exp e1 tp >>= fun () -> check_exp e2 tp >>= fun () -> return tp *)
    | _ -> failwith "tc: unsupported binaryop"
    end
  (* | BinaryOp (e1, op, e2) -> synth e1 >>= fun tp1 -> synth e2 >>= fun tp2 -> return () *)
  | _ -> failwith "unsupported synth"

(* construct -  return context *)  
and check_exp (e: exp) (tp: typ) : unit t = match e with
  | IfElseExp (e1, c, e2) -> check_exp c (TBool def_seg) >>= 
    fun () -> check_exp e1 tp >>= fun () -> check_exp e2 tp
  | Lst [] -> return ()
  | Lst (e::rest) -> check_exp e tp >>= fun () -> check_exp (Lst rest) tp
  | e -> synth_exp e >>= typ_eq tp (* TODO: replace with subtyping *)

and synth_params pl = match pl with (* return list of types *)
  | [] -> return []
  | (_, Typ tp)::rest -> synth_params rest >>= fun tp_args -> return (tp::tp_args)
  | _ -> assert false

and synth_stmt s = match s with
  | Return e -> synth_exp e
  | Exp e -> synth_exp e
  | _ -> failwith "unsupported synth stmt"

and synth_stmt_lst sl = match sl with
  | [] -> return (TNone def_seg)
  | (Return e)::_ -> synth_exp e
  | s::rest -> check_stmt s >>= fun () -> synth_stmt_lst rest

and check_stmt s = match s with
  | Assign (Some (Typ tp), _::[], e::[]) -> check_exp e tp
  (* | Assign (None, _::[], e::[]) -> synth_exp e if exists, check. else use rhs. extend context. *)
  | Function (_, _, pl, Typ tp, sl) -> synth_params pl >>= fun _ -> synth_stmt_lst sl >>= fun sltp -> typ_eq tp sltp
  | Assert e -> check_exp e (TBool def_seg)
  | Break -> return ()
  | Continue -> return ()
  | Pass -> return ()
  (* | s -> synth_stmt s >>= typ_eq *)
  | _ -> failwith "unsupported check stmt"

(* synth pl, extend context with type of function, typecheck body with params introduced *)
(* types of all functions should be available in context *)


let map f m ctx =
  let open Options in
  m ctx >>= fun (x, ctx) -> 
    return (f x, ctx)
  
let check_prog (Program sl) = match sl with
  | s1::[] -> begin match check_stmt s1 [] with | Some ((), ctx) -> print ctx | None -> failwith "program untyped" end
  | _ -> failwith ""