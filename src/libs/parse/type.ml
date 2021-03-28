(* open Base
open Astpy
open Sourcemap

module type TYPING = sig
  type ctx = (identifier * typ option) list
  type 'a t = ctx -> ('a * ctx) option

  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'b -> ('a * 'b) option
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val synth : exp -> typ t
  val check : exp -> typ -> unit t

  val fail : 'a t
  val check_var : identifier -> typ -> unit t
  val lookup : identifier -> typ t
  val with_var : identifier -> 'a t -> 'a t
  val typ_eq : typ -> typ -> unit t
end

module Typing : TYPING = struct
  type ctx = (identifier * typ option) list
  type 'a t = ctx -> ('a * ctx) option

  let map f m ctx =
    let open Option in
    m ctx >>= fun (x, ctx) -> 
      return (f x, ctx)
  
  let return x = fun ctx -> Some (x, ctx)
  let (>>=) m f = fun ctx -> 
    let open Option in
    m ctx >>= fun (a, ctx') -> f a ctx'

  let fail = fun _ -> None

  let rec check_var x tp = fun ctx ->
    let open Option in
    match ctx with
    | [] -> fail (* out-of-scope variable reference *)
    | (y, None)::rest when x = y -> return ((), (y, Some tp)::rest)
    | (y, Some _)::rest when x = y -> fail (* variables should be single-use *)
    | h::rest -> check_var x tp rest >>= fun ((), rest') -> 
      return ((), h::rest')

  let lookup x = fun ctx ->
    match List.assoc_opt x ctx with
    | None -> Option.fail
    | Some None -> Option.fail
    | Some (Some tp) -> Option.return (tp, ctx)

  let with_var x m = fun ctx -> 
    let open Option in
    m ((x, None)::rest) >>= function
    | (r, (y, Some _)::rest') when x = y -> return (r, rest') 
    | (r, (y, None)::rest') when x = y -> fail (* type of variable should have been set *)
    | _ -> assert false

  let typ_eq tp1 tp2 = if typ_compare tp1 tp2 = 0 then return () else fail

  let rec synth = function
  | Literal BoolLit -> Bool

end *)