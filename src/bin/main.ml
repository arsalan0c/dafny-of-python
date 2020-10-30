open Base

let printf = Stdlib.Printf.printf
let inp = Stdio.In_channel.input_all Stdio.stdin

let () = begin
  printf "\nParsing\n\"%s\"\n\r" inp;
  inp
  |> Lexing.from_string 
  |> Pyparse.Parser.f Pyparse.Indenter.f
  (* |> Pyparse.Ast.prog_str *)
  |> Verifier.Hoare.prog_z3
  |> printf "->\n%s\n"
end
