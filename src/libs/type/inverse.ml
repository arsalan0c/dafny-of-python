(* type tp = One | Tensor of tp * tp | Lolli of tp * tp 
type var = string

type exp = 
  | Unit | LetUnit of exp * exp 
  | Pair of exp * exp | LetPair of var * var * exp * exp 
  | Lam of var * exp | App of exp * exp 
  | Var of var 

module Option = struct
  type 'a t = 'a option
  let map f = function
    | None -> None
    | Some x -> Some (f x)
  let return x = Some x 
  let fail = None
  let (>>=) m f = 
    match m with
    | None -> None
    | Some x -> f x
end

module type TYPING =  sig
  type ctx = (var * tp option) list
  type 'a t 

  val synth : exp -> tp t
  val check : exp -> tp -> unit t

  val map : ('a -> 'b) -> 'a t -> 'b t 
  val return : 'a -> 'b -> ('a * 'b) option
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val checkvar : var -> tp -> unit t
  val lookup : var -> tp t 
  val withvar : var -> 'a t -> 'a t
  val tp_eq : tp -> tp -> unit t

  val run : 'a t -> ctx -> ('a * ctx) option 
end

              
module Typing : TYPING = struct
  type ctx = (var * tp option) list 

  type 'a t = ctx -> ('a * ctx) option 

  let map f m ctx = 
    let open Option in 
    m ctx >>= fun (x, ctx) -> 
    return (f x, ctx)

  let return x = fun ctx -> Some(x, ctx)

  let (>>=) m f = fun ctx -> 
    let open Option in 
    m ctx >>= fun (a, ctx') -> 
    f a ctx'

  let fail : 'a t = fun ctx -> None 

  let rec checkvar (x : var) (tp : tp) : unit t = fun ctx -> 
    let open Option in 
    match ctx with
    | [] -> fail 
    | (y, None) :: rest when x = y -> return ((), (y, Some tp) :: rest)
    | (y, Some _) :: rest when x = y -> fail
    | h :: rest -> checkvar x tp rest >>= fun ((), rest') -> 
                   return ((), h :: rest')

  let lookup x (ctx : ctx) = 
    match List.assoc_opt x ctx with
    | None -> Option.fail
    | Some None -> Option.fail
    | Some (Some tp) -> Option.return(tp, ctx)

  let withvar (type a) (x : var) (m : a t) : a t = fun ctx -> 
    let open Option in 
    m ((x, None) :: ctx) >>= function
    | (r, (y, (Some _)) :: ctx') when x = y -> return (r, ctx')
    | (r, (y, None) :: ctx') when x = y -> fail 
    | (r, (y, _) :: ctx') -> assert false 
    | (r, []) -> assert false

  let tp_eq (tp1 : tp) (tp2 : tp) = if tp1 = tp2 then return () else fail 

  let rec synth = function
    | Unit -> return One
    | Pair(e1, e2) -> synth e1 >>= fun tp1 -> 
                      synth e2 >>= fun tp2 -> 
                      return (Tensor(tp1, tp2))
    | Lam(x, e) -> synth e >>= fun ret_tp -> 
                   lookup x >>= fun arg_tp -> 
                   return (Lolli(arg_tp, ret_tp))
    | LetUnit(e, e') -> check e One >>= fun () -> 
                        synth e'
    | LetPair(x, y, e, e') -> 
       withvar y (withvar x (synth e' >>= fun res_tp -> 
                             lookup x >>= fun tp1 -> 
                             lookup y >>= fun tp2 -> 
                             check e (Tensor(tp1, tp2)) >>= fun () -> 
                             return res_tp))
    | App(_, _) -> fail 
    | Var _ -> fail 
  and check (e : exp) (tp : tp) : unit t = 
    match e with 
    | Var x -> checkvar x tp 
    | App(e1, e2) -> synth e2 >>= fun tp_arg -> 
                     check e1 (Lolli(tp_arg, tp))
    | e -> synth e >>= tp_eq tp

  let run m = m 
end *)