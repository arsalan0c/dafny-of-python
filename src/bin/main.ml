open Core
(* open Sexplib *)

type result = Success of string | Fail of string

let printf = Stdlib.Printf.printf
let prerr = Stdlib.prerr_string

let list_dfy = "./src/libs/parse/list.dfy"
let prelude_f = "./src/libs/parse/prelude.dfy" 
let dafny_f = "program.dfy"
let dafny_command = String.concat ~sep:" " ["dafny"; dafny_f; prelude_f; list_dfy]
let python_f = "program.py"
let mypy_command = String.concat ~sep:" " ["mypy"; python_f]

let system cmd =
  let inp = Unix.open_process_in cmd in
  let out = In_channel.input_lines inp in
  In_channel.close inp; 
  String.concat ~sep:"\n" out

let typcheck s = 
  Out_channel.write_all python_f ~data:s;
  let tc_out = system mypy_command in
  match String.substr_index tc_out ~pattern:"error" with
  | Some _ -> prerr ("\nTypechecking failed:\n" ^ tc_out ^ "\n")
  | None -> ()

let () = begin
<<<<<<< HEAD
  Pyparse.Parser.pp_exceptions ();
  let inp = Stdio.In_channel.input_all Stdio.stdin in
  typcheck inp;
  let parsed = Pyparse.Parser.parse_string inp in
  (* Pyparse.Typing.check_prog parsed *)
  (* let sexp = Pyparse.Astpy.sexp_of_program parsed in *)
  (* printf "->\n%s\n" (Sexp.to_string sexp); *)
  let dafny_ast = Pyparse.Todafnyast.prog_dfy parsed in
  let dafny_source = Pyparse.Emitdfy.print_prog dafny_ast in
  printf "\n%s\n" dafny_source; 
  Out_channel.write_all dafny_f ~data:dafny_source;
  let verification_out = system dafny_command in
  Pyparse.Report.report verification_out
=======
  printf "\nParsing\n\"%s\"\n\r" inp;
  inp
  |> Lexing.from_string 
  |> Pyparse.Parser.f Pyparse.Indenter.f
  |> Pyparse.Ast.prog_str
  (* |> Verifier.Hoare.prog_z3 *)
  |> printf "->\n%s\n"
>>>>>>> a3e90417837db0a6eb6b53e8e72ac3e32e2a499d
end
 