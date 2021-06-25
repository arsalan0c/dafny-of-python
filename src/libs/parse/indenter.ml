(* Source: https://gist.github.com/zehnpaard/124a9c6df632839d01b4fede8684ddd8 *)

open Menhir_parser

let convert_space_to_indent width f =
	let indent = ref 0 in
	let make_indent _ = [NEWLINE; INDENT] in
	let make_dedent _ = [NEWLINE; DEDENT] in
	let g h a b = List.init (a - b) h |> List.concat in

	fun lexbuf -> match f lexbuf with
	| SPACE n ->
		let m = n / width in
		let k = !indent in
		if m > k then (indent := m; g make_indent m k)
		else if m < k then (indent := m; g make_dedent k m)
		else [NEWLINE]
	| EOF ->
		let k = !indent in
		(indent := 0; g make_dedent k 0 @ [EOF])
	| TYPF ident -> [IDENTIFIER ident; LPAREN] 
	| e -> [e]

let flatten f =
	let xs = ref [] in
		fun lexbuf -> match !xs with
		| x::xs' -> xs := xs'; x
		| [] -> (match f lexbuf with
			| x::xs' -> xs := xs'; x
			| [] -> failwith "Lexer did not return EOF token")

let f = Lexer.next_token |> convert_space_to_indent 2 |> flatten
