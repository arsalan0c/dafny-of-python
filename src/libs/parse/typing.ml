(* open Base
open Astpy
open Sourcemap
open Sexplib

(* module type TYPING = sig
  type var = string
  type ctx = (var * typ Options) list
  type 'a t = ctx -> ('a * ctx) Options

  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'b -> ('a * 'b) Options
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

let map f m ctx =
  let open Options in
  m ctx >>= fun (x, ctx) -> 
    return (f x, ctx)

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

let lookup x = fun (ctx: ctx) ->
  match List.Assoc.find ctx ~equal:(fun x y -> String.compare x y = 0) x with
  | None -> Options.fail
  | Some None -> Options.fail
  | Some (Some tp) -> Some tp (* Options.return (tp, ctx) *)

let with_var (type a) (x: var) (m: a t) : a t = fun ctx -> 
  let open Options in
  m ((x, None)::ctx) >>= function
  | (r, (y, Some _)::ctx') when String.compare x y = 0 -> return (r, ctx') 
  | (_, (y, None)::_) when String.compare x y = 0 -> failwith "" (* type of variable should have been set *)
  | _ -> assert false

(* use *)
let rec synth e = match e with
  | Typ tp -> return tp
  | Identifier ident -> lookup (segment_value ident) ctx
  | Literal BoolLit _ -> return (Bool default_segment)
  | UnaryOp (Not, _) -> return (Bool default_segment)
  (* | BinaryOp (e1, op, e2) -> synth e1 >>= fun tp1 -> synth e2 >>= fun tp2 -> return () *)
  | _ -> failwith "unsupported synth"

(* construct *)  
and check (e: exp) (tp: typ) : unit t = match e with
  (* | Identifier ident -> check_var (segment_value ident) tp *)
  | BinaryOp (e1, op, e2) -> 
    match op with
    | Plus _ -> match check e1 tp, check e2 tp2 with | Some
    | And _ ->  match check e1 tp, check e2 tp2 with | Some
  | IfElseExp (e1, c, e2) -> begin 
    match check c (Bool default_segment), check e1 tp, check e2 tp with 
    | Some (Bool _), Some _, Some _ -> return tp
    | _ -> None
    end
  | e -> synth e >>= typ_eq tp (* TODO: replace with subtyping *)

end *)
