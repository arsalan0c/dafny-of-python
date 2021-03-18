open Base
open Astpy

let printf = Stdlib.Printf.printf

let convert_typvar ident call = match ident, call with
  | Identifier ident, Call (c_ident, (Literal (StringLit tv_v))::tv_rest) -> begin
    let ident_v = Sourcemap.segment_value ident in 
    let c_ident_v = Sourcemap.segment_value c_ident in
    match c_ident_v with 
    | "TypeVar" -> begin match tv_rest with 
      | [] -> if String.compare ident_v tv_v = 0 then Some tv_v else failwith "Argument to TypeVar must match declared variable"
      | _ -> failwith "Please provide a single argument to TypeVar, a string with just the name of the declared variable. Constrained type variables are not (yet) supported." 
      end
    | _ ->  None
    end
  | _ -> None

let generics = function
  |  Assign (t, il, el) -> begin
    match el with | ((Call ((_, Some "TypeVar") , _))::_) -> begin
      match List.map2 ~f:convert_typvar il el with
      | Ok typ_vars -> let vs = List.filter_map ~f:(fun x -> x) typ_vars in (None, vs)
      | Unequal_lengths -> failwith "Number of left-hand identifiers in assignment must be equal to number of right-hand expressions"
    end
    | _ -> (Some (Assign (t, il, el)), [])
  end
  | s -> (Some s, [])

let prog = function 
  | Program sl ->
    let (n_osl, vs) = List.fold sl ~f:(
      fun (sf_sl, sf_gens) s -> let (s, gens) = generics s in (sf_sl@[s], sf_gens@gens)) ~init:([], []) in
    let n_sl = List.filter_map ~f:(fun x -> x) n_osl in
    (Program n_sl, vs)

  