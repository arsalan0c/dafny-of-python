open Base

let printf = Stdlib.Printf.printf
let inp = Stdio.In_channel.input_all Stdio.stdin

let () = begin
  printf "\nTrying to parse\n\"%s\"\n\r" inp;
  inp
  |> Lexing.from_string 
  |> Pyparse.Parser.f Pyparse.Indenter.f
  |> Pyparse.Ast.prog_str
  |> printf "->\n%s\n"
end
