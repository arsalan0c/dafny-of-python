open Base
open Astpy

let printf = Stdlib.Printf.printf
let vars = Hash_set.create (module String)

let convert_typvar lhs rhs = 
  match lhs with
  | Identifier (_, Some ident_v) -> begin
    match rhs with 
    | Call ((Identifier (_, Some c_ident_v)), ((Literal (StringLit tv_v))::tv_rest)) -> begin match c_ident_v with 
      | "TypeVar" -> begin match tv_rest with 
        | [] -> if String.compare ident_v tv_v = 0 then (Hash_set.add vars tv_v; Some tv_v) else failwith "Please provide a single argument to TypeVar, a string equivalent to the name of the declared variable."
        | _ -> failwith "Please provide a single argument to TypeVar, a string equivalent to the name of the declared variable. Constrained type variables are not supported (yet)." 
        end
      | _ ->  None
      end
    | Identifier (_, Some var) -> begin 
        match Base.Hash_set.find vars ~f:(fun s -> String.compare s var = 0) with
        | Some _ -> Hash_set.add vars ident_v; Some ident_v
        | None -> None
      end
    | _ -> None
    end
  | _ -> None

let generics = function
  |  Assign (t, il, el) -> begin
    match List.map2 ~f:convert_typvar il el with
    | Ok typ_vars -> let vs = List.filter_map ~f:(fun x -> x) typ_vars in if List.length vs > 0 then (None, vs) else (Some (Assign (t, il, el)), vs)
    | Unequal_lengths -> failwith "Number of left-hand identifiers in assignment must be equal to number of right-hand expressions"
    end
  | s -> (Some s, [])

let prog = function 
  | Program sl ->
    let (n_osl, vs) = List.fold sl ~f:(
      fun (sf_sl, sf_gens) s -> let (s, gens) = generics s in (sf_sl@[s], sf_gens@gens)) ~init:([], []) in
    let n_sl = List.filter_map ~f:(fun x -> x) n_osl in
    (Program n_sl, vs)
