include Nice_parser.Make(struct
  type result = Astpy.program
  type token = Menhir_parser.token
  exception ParseError = Menhir_parser.Error
  let indent = Indenter.f
  let parse = Menhir_parser.program
  include Lexer
end)


(*===========================================================================*)
(* TESTS                                                                     *)
(* https://dune.readthedocs.io/en/stable/tests.html#inline-expectation-tests *)
(*===========================================================================*)

let%test_module _ = (module struct
  
  let () = Printexc.record_backtrace false

end)
