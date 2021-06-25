(* open Base
(* open Sexplib *)
open Astpy
open Sourcemap


type var = string
type ctx = (var * typ) list

let add_binding ctx x tp = (x, tp)::ctx
let get_binding ctx v = List.Assoc.find ~equal:(fun x y -> String.compare x y = 0) v

let typ_eq tp1 tp2 = typ_compare tp1 tp2 = 0
let typ_cp tp1 tp2 res msg = 
  if typ_eq tp1 tp2 then (tp1, res) else failwith msg

let rec typof_exp ctx term = 
  match term with
  | Typ t -> (t, term)
  | Identifier _ -> (get_binding seg ctx, term)
  | Literal (BoolLit _ ) -> (TBool def_seg, term)
  | BinaryExp (e1, op, e2) -> begin match op with
    | Plus _ -> let tp1 = typ_cp (fst (typof_exp ctx e1) (typof_exp ctx e2) term "tc: illegal plus operands"
    | _ -> failwith "tc: unsupported binaryop"
    end
  | _ -> failwith "tc: unsupported exp"

let rec typof_stmt ctx term =
  match term with
  | Assign (Some tp, ident::[], e::[]) -> let res = typ_cp (typof_exp ctx e) term "tc: incorrect assign annotation" in add_binding
  | Assign (None, ident::[], e::[]) -> match get_binding ctx ident | Some tp -> typ_cp tp (typof_exp ctx e) term "tc: incorrect assign annotation" (* if exists, check. else use rhs*)
  | Function 
     *)
