open Base

let printf = Stdlib.Printf.printf
let inp = Stdio.In_channel.input_all Stdio.stdin

let () = begin
  printf "\nTrying to parse \"%s\".\n\r" inp;
  inp
  |> Lexing.from_string 
  |> Pyparse.Parser.f Pyparse.Indenter.f
  |> Pyparse.Ast.sexp_of_sexp
  |> Sexp.to_string_hum
  |> printf "-> %s\n"
  
end
