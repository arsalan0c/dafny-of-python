open Core
(* open Sexplib *)

(* let parse lexbuf = try Success (Pyparse.Translate.prog_str (Pyparse.Parser.f Pyparse.Indenter.f lexbuf)) with
  | Pyparse.Parser.Error -> Fail(String.concat ~sep:", " ["Parser error"; (Pyparse.Sourcemap.print_pos (Lexing.lexeme_end_p lexbuf)); Lexing.lexeme lexbuf; "\n"]) *)
(* 
let parse_dfyast prelude lexbuf = try Success (prelude ^ "" ^ (Pyparse.Emitdfy.print_prog (Pyparse.Todafnyast.prog_dfy (Pyparse.Convertcall.prog (Pyparse.Parser.f Pyparse.Indenter.f lexbuf))))) with
  | Pyparse.Parser.Error -> Fail(String.concat ~sep:", " ["Parser error"; (Pyparse.Sourcemap.print_pos (Lexing.lexeme_end_p lexbuf)); Lexing.lexeme lexbuf; "\n"]) *)

type result = Success of string | Fail of string

let printf = Stdlib.Printf.printf
let prerr = Stdlib.prerr_string

let prelude_f = "./src/libs/parse/prelude.dfy" 
let dafny_f = "pyny_program.dfy"
let dafny_command = String.concat ~sep:" " ["dafny"; dafny_f] (* prelude_f *)
let python_f = "pyny_program.py"
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
  | Some _ -> prerr "Typechecking failed:%s\n"
  | None -> ()

let run =
  let inp = Stdio.In_channel.input_all Stdio.stdin in
  typcheck inp;
  let lexed = Lexing.from_string inp in
  let parsed = Pyparse.Parser.f Pyparse.Indenter.f lexed in
  let calls_rewritten = Pyparse.Convertcall.prog parsed in
  let dafny_ast = Pyparse.Todafnyast.prog_dfy calls_rewritten in
  let dafny_source = Pyparse.Emitdfy.print_prog dafny_ast in
  printf "\n%s\n" dafny_source; 
  Out_channel.write_all dafny_f ~data:dafny_source;
  let verification_out = system dafny_command in
  Pyparse.Report.report verification_out

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
