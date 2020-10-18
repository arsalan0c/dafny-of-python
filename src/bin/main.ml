open Base

let printf = Stdlib.Printf.printf

let () = begin
  
  Lexing.from_channel Stdio.stdin
  |> Pyparse.Parser.f Pyparse.Indenter.f
  |> Pyparse.Ast.sexp_of_sexp
  |> Sexp.to_string_hum
  |> printf "\n%s\n"
  
end
