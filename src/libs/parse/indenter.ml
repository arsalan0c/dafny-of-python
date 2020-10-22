(* Source: https://gist.github.com/zehnpaard/124a9c6df632839d01b4fede8684ddd8 *)

module P = Parser

let convert_space_to_indent width f =
	let indent = ref 0 in
	let make_indent _ = [P.NEWLINE; P.INDENT] in
	let make_dedent _ = [P.NEWLINE; P.DEDENT] in
	let g h a b = List.init (a - b) h |> List.concat in

	fun lexbuf -> match f lexbuf with
	| P.SPACE n ->
		let m = n / width in
		let k = !indent in
		if m > k then (indent := m; g make_indent m k)
		else if m < k then (indent := m; g make_dedent k m)
		else [P.NEWLINE]
	| P.EOF ->
		let k = !indent in
	  (indent := 0; g make_dedent k 0 @ [P.EOF])
	| e -> [e]

let flatten f =
	let xs = ref [] in
		fun lexbuf -> match !xs with
		| x::xs' -> xs := xs'; x
		| [] -> (match f lexbuf with
			| x::xs' -> xs := xs'; x
			| [] -> failwith "Lexer did nto return EOF token")

let f = Lexer.f |> convert_space_to_indent 2 |> flatten


