{
  open Parser

  (* TODO: emit space for comments *)

  exception LexError of string
  let printf = Stdlib.Printf.printf
  let[@inline] failwith msg = raise (LexError msg)
  let[@inline] illegal c =
    failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)

  let strip_quotes str =
    match String.length str with
    | 0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)
  
  let pring = function
    | Some s -> s
    | None -> ""

  let emit_segment lb v = 
    let s = Lexing.lexeme_start_p lb in
    (* printf "Seg: %s, %s\n" (Sourcemap.print_pos s) (pring v); *)
    (s, v)

  let upd (lb: Lexing.lexbuf) cols =
    let lcp = lb.lex_curr_p in
    lb.lex_curr_p <- { lcp with
      pos_lnum = lcp.pos_lnum + 1;
      pos_cnum = lcp.pos_cnum + cols;
      pos_bol = lcp.pos_cnum - cols;
    }   
}

let indent = '\n' ' '*
let whitespace = [' ' '\t']

(* simple types *)
let int_type = "int"
let float_type = "float"
let complex_type = "complex" 
let bool_type = "bool"
let string_type = "str"
let none_type = "None"

(* data types *)
let list_type = "list"
let dict_type = "dict"
let set_type = "set"
let tuple_type = "Tuple"

(* let type = simple_type | data_type | data_type '[' type+ ']'  *)

let identifier = ['a'-'z' 'A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let digit = ['0'-'9']
let integer = (digit | ['1' - '9'] digit*)
let stringliteral = ('"'[^'"''\\']*('\\'_[^'"''\\']*)*'"')
let comment = '#'
let boolean = "True" | "False"
let e = ""

rule f = parse
| eof { EOF }
| int_type as t { INT_TYPE (emit_segment lexbuf (Some t)) }
| float_type as t { FLOAT_TYPE (emit_segment lexbuf (Some t)) }
| bool_type as t { BOOL_TYPE (emit_segment lexbuf (Some t)) }
| complex_type as t { COMPLEX_TYPE (emit_segment lexbuf (Some t)) }
| string_type as t { STRING_TYPE (emit_segment lexbuf (Some t)) }
| none_type as t { NONE_TYPE (emit_segment lexbuf (Some t)) }
| list_type as t { LIST_TYPE (emit_segment lexbuf (Some t)) }
| dict_type as t { DICT_TYPE (emit_segment lexbuf (Some t)) }
| set_type as t { SET_TYPE (emit_segment lexbuf (Some t)) }
| tuple_type as t { TUPLE_TYPE (emit_segment lexbuf (Some t)) }
| indent as s { (upd lexbuf (String.length s - 1); SPACE (String.length s - 1)) }
| whitespace+ { f lexbuf }
| "import" { comment lexbuf }
| "from" { comment lexbuf }
| "# pre" { PRE }
| "# post" { POST }
| "#pre" { PRE }
| "pre" { PRE }
| "#post" { POST }
| "# invariant" { INVARIANT }
| "#invariant" { INVARIANT }
| "#decreases" { DECREASES }
| "# decreases" { DECREASES }
| "forall" { FORALL }
| "exists" { EXISTS }
| "<==>" { BIIMPL (emit_segment lexbuf (Some "<==>" )) }
| "==>" { IMPLIES (emit_segment lexbuf (Some "==>" )) }
| "<==" { EXPLIES (emit_segment lexbuf (Some "<==" )) }
| "::" { DOUBLECOLON }
| comment { comment lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '['  { LBRACK }
| ']' { RBRACK }
| ':' { COLON }
| ';' { SEMICOLON }
| ',' { COMMA }
| "len" { LEN }
| "filter" { IDENTIFIER (emit_segment lexbuf (Some "filterF")) }
| "map" { IDENTIFIER (emit_segment lexbuf (Some "mapF")) }
| "->" { ARROW }
| "def" { DEF (emit_segment lexbuf (Some "def" )) }
| "if" { IF (emit_segment lexbuf (Some "if" )) }
| "elif" { ELIF (emit_segment lexbuf (Some "elif" )) }
| "else" { ELSE (emit_segment lexbuf (Some "else" )) }
| "for" { FOR (emit_segment lexbuf (Some "for" )) }
| "while" { WHILE (emit_segment lexbuf (Some "while" )) }
| "break" { BREAK (emit_segment lexbuf (Some "break" )) }
| "continue" { CONTINUE (emit_segment lexbuf (Some "continue")) }
| "pass" { PASS (emit_segment lexbuf (Some "pass")) }
| "print" { PRINT (emit_segment lexbuf (Some "print")) }
| "return" { RETURN (emit_segment lexbuf (Some "return")) }
| "assert" { ASSERT (emit_segment lexbuf (Some "assert")) }
| "in" { IN (emit_segment lexbuf (Some "in")) }
| "==" { EQEQ (emit_segment lexbuf (Some "==")) }
| '=' { EQ (emit_segment lexbuf (Some "=")) }
| "!=" { NEQ (emit_segment lexbuf (Some "!=")) }
| '+' { PLUS (emit_segment lexbuf (Some "+")) }
| "+=" { PLUSEQ (emit_segment lexbuf (Some "+=")) }
| '-' { MINUS (emit_segment lexbuf (Some "-")) }
| "-=" { MINUSEQ (emit_segment lexbuf (Some "-=")) }
| '*' { TIMES (emit_segment lexbuf (Some "*")) }
| "*=" { TIMESEQ (emit_segment lexbuf (Some "*=")) }
| "/" { DIVIDE (emit_segment lexbuf (Some "/")) }
| "/=" { DIVIDEEQ (emit_segment lexbuf (Some "/=")) }
| "%" { MOD (emit_segment lexbuf (Some "%")) }     
| "<=" { LEQ (emit_segment lexbuf (Some "<=")) }
| '<' { LT (emit_segment lexbuf (Some "<")) }
| ">=" { GEQ (emit_segment lexbuf (Some ">=")) }
| '>' { GT (emit_segment lexbuf (Some ">")) }
| "and" { AND (emit_segment lexbuf (Some "and")) }
| "or" { OR (emit_segment lexbuf (Some "or")) }
| "not" { NOT (emit_segment lexbuf (Some "not")) }
| "True" { TRUE }
| "False" { FALSE }
| "None" { NONE  }
| integer as i { INT (int_of_string i) }
| identifier as i { IDENTIFIER (emit_segment lexbuf (Some i)) }
| stringliteral as s { STRING (strip_quotes s) }
| _ as c { illegal c }

and comment = parse
| indent as s { (upd lexbuf (String.length s - 1); f lexbuf) } 
| _ { comment lexbuf }
