open Core
(* open Sexplib *)

(* Process support is mostly in the "unix" library. *)
(* open Unix *)

type result = Success of string | Fail of string

let printf = Stdlib.Printf.printf
let inp = Stdio.In_channel.input_all Stdio.stdin

let prelude = Stdio.In_channel.read_all "./src/libs/parse/prelude.dfy" 

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

let dfy_filename = "runsample.dfy"
let command_dfy = String.concat ~sep:" " ["dafny"; dfy_filename] 

let py_filename = "inp.py"
let command_mypy = String.concat ~sep:" " ["mypy"; py_filename]

let mt_fn = "inp"
let command_mt = String.concat ~sep:" " ["monkeytype apply"; mt_fn]

let opt_get os = match os with
  | Some(s) -> s
  | None -> "none"

let replace_num p =
  let nums = (Re2.find_all_exn (Re2.create_exn "[0-9]*") p) in
  let line_column = List.filter ~f:(fun s -> 
    let s = String.length s in if s = 0 then false else true) nums in
  let line = begin
    match (List.nth line_column 0) with
    | Some s -> int_of_string s
    | None -> 0
    end in
  let column = begin
    match (List.nth line_column 1) with
    | Some s -> int_of_string s
    | None -> 0
    end in
  let seg = Pyparse.Astdfy.nearest_seg (!Pyparse.Astdfy.sm) line column in
  let seg_str = Pyparse.Sourcemap.print_segment seg in seg_str
  (* (printf "Here:\n%s\n%d %d\n" (Pyparse.Ast.print_sm !Pyparse.Ast.sm) line column; seg_str) *)

  (* let res = String.concat ~sep:", " line_column in
  (printf "\n%s\n\n" res; res) *)

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if (String.compare (String.sub s1 ~pos:i ~len:len) s2 = 0) then raise Exit
    done;
    false
  with Exit -> true

let lst_lst_str olst = 
  let replaced_nums = List.map ~f:(fun lst -> List.mapi ~f:(fun i s -> if i = 0 then replace_num s else s) lst) olst in 
  let first = List.map ~f:(fun lst -> String.concat ~sep:", " lst) replaced_nums in
  String.concat ~sep:"\n" first

(* let parse lexbuf = try Success (Pyparse.Translate.prog_str (Pyparse.Parser.f Pyparse.Indenter.f lexbuf)) with
  | Pyparse.Parser.Error -> Fail(String.concat ~sep:", " ["Parser error"; (Pyparse.Sourcemap.print_pos (Lexing.lexeme_end_p lexbuf)); Lexing.lexeme lexbuf; "\n"]) *)

let parse_dfyast prelude lexbuf = try Success (prelude ^ "\n" ^ (Pyparse.Astdfy.print_prog (Pyparse.Todafnyast.prog_dfy (Pyparse.Convertcall.prog (Pyparse.Parser.f Pyparse.Indenter.f lexbuf))))) with
  | Pyparse.Parser.Error -> Fail(String.concat ~sep:", " ["Parser error"; (Pyparse.Sourcemap.print_pos (Lexing.lexeme_end_p lexbuf)); Lexing.lexeme lexbuf; "\n"])

let write_to_file s f c = Out_channel.write_all f ~data:s ; String.concat ~sep:"\n" (run c)

let final s =
  (* printf "%s\n" s; *)
  let line_rgx = Re2.create_exn "\([0-9]*,[0-9]*\).+" in
  let lines = Re2.find_all_exn line_rgx s in 
  let split_rgx = Re2.create_exn ":" in
  let split_f s = Re2.split split_rgx s in
  let split_lines = List.map ~f:split_f lines in 
  printf "%s\n" (lst_lst_str split_lines)

let main =
  (* let inf = write_to_file inp py_filename command_mt in
  printf "\nType inferencing:%s\n" inf; inferencing *)
  (* let tc = write_to_file inp py_filename command_mypy in
  if (contains tc "error") then 
  printf "\nTypechecking:%s\n" tc else *)
  (printf "\nParsing\n\"%s\"\n\r" inp;
  let res = parse_dfyast "" (Lexing.from_string inp) in 
  printf "\n%s\n" (Pyparse.Astdfy.print_sm !Pyparse.Astdfy.sm);
  match res with
  | Success s ->  printf "%s" s; let f = write_to_file s dfy_filename command_dfy in final f
  | Fail s -> printf "%s" s
  )
(* 
let main2 =
  printf "\nParsing\n\"%s\"\n\r" inp;
  let res = Pyparse.Astpy.sexp_of_sexp (Pyparse.Parser.f Pyparse.Indenter.f (Lexing.from_string inp)) in 
  let se = Sexp.to_string res in
  printf "->\n%s\n" se *)

(* let main_dfy_sexp =
  printf "\nParsing\n\"%s\"\n\r" inp;
  let res = Pyparse.Astdfy.sexp_of_dProgram (Pyparse.Todafnyast.prog_dfy (Pyparse.Parser.f Pyparse.Indenter.f (Lexing.from_string inp))) in 
  let se = Sexp.to_string res in
  printf "->\n%s\n" se *)

(* 
  inp
  |> Lexing.from_string 
  |> parse
  (* |> (fun s -> (let _ = Pyparse.Ast.prog_str s in Pyparse.Ast.printtbl Pyparse.Ast.sm)) *)
  |> 
  (*|> Verifier.Hoare.prog_z3 *)
  (* |> Sys.command "dafny %s" *)
  (* |>  *)
  (* |> printf "->\n%s\n" *)
  |>  *)
(* end *)
