{
  open Menhir_parser

  exception LexError of string
  let printf = Stdlib.Printf.printf
  let[@inline] failwith msg = raise (LexError msg)
  let[@inline] illegal c =
    failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)

  let strip_quotes str =
    match String.length str with
    | 0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)

  let emit_segment lb v = 
    let s = Lexing.lexeme_start_p lb in
    (s, v)

  let next_line (lb: Lexing.lexbuf) cols =
    let lcp = lb.lex_curr_p in
    lb.lex_curr_p <- { lcp with
      pos_lnum = lcp.pos_lnum + 1;
      pos_cnum = lcp.pos_cnum + cols;
      pos_bol = lcp.pos_cnum - cols;
    }   
}

let indent = '\n' [' ' '\t']*
let whitespace = [' ' '\t']+

(* simple types *)
let int_typ = "int"
let float_typ = "float"
let bool_typ = "bool"
let str_typ = "str"
(* let none_typ = "None" *)
let obj_typ = "object"

(* complex types *)
let list_typ = "list"
let dict_typ = "dict"
let set_typ = "set"
let tuple_typ = "tuple"
let callable_typ = "Callable"
let type_typ = "Type"
let union_typ = "Union"

let typ = int_typ | float_typ | bool_typ | str_typ | obj_typ | list_typ | dict_typ | set_typ
let typ_f = typ '('

let identifier = ['a'-'z' 'A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let digit = ['0'-'9']
let integer = '-'? digit digit*
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = frac exp | digit+ exp | digit+ frac exp | digit* frac
let strliteral = ('"'[^'"''\\']*('\\'_[^'"''\\']*)*'"')
let comment = '#'
let boolean = "True" | "False"

let pre = '#' [' ' '\t']* "pre"
let post = '#' [' ' '\t']* "post"
let invariant = '#' [' ' '\t']* "invariant"
let decreases = '#' [' ' '\t']* "decreases"
let reads = '#' [' ' '\t']* "reads"
let modifies = '#' [' ' '\t']* "modifies"

rule next_token = parse
| eof { EOF }
| int_typ as t { INT_TYP (emit_segment lexbuf (Some t)) }
| float_typ as t { FLOAT_TYP (emit_segment lexbuf (Some t)) }
| bool_typ as t { BOOL_TYP (emit_segment lexbuf (Some t)) }
| str_typ as t { STRING_TYP (emit_segment lexbuf (Some t)) }
| obj_typ as t { OBJ_TYP (emit_segment lexbuf (Some t)) }
| list_typ { LIST_TYP (emit_segment lexbuf (Some "List")) }
| dict_typ as t { DICT_TYP (emit_segment lexbuf (Some t)) }
| set_typ as t { SET_TYP (emit_segment lexbuf (Some t)) }
| tuple_typ as t { TUPLE_TYP (emit_segment lexbuf (Some t)) }
| callable_typ as t { CALLABLE_TYP (emit_segment lexbuf (Some t)) }
| type_typ as t { TYPE_TYP (emit_segment lexbuf (Some t)) }
| union_typ as t { UNION_TYP (emit_segment lexbuf (Some t)) }
| indent as s { (next_line lexbuf (String.length s - 1); SPACE (String.length s - 1)) }
| "import" { comment lexbuf }
| "from" { comment lexbuf }
| pre { PRE }
| post { POST }
| invariant { INVARIANT }
| decreases { DECREASES }
| reads { READS }
| modifies { MODIFIES }
| "forall" { FORALL }
| "exists" { EXISTS }
| "<==>" { BIIMPL (emit_segment lexbuf (Some "<==>" )) }
| "==>" { IMPLIES (emit_segment lexbuf (Some "==>" )) }
| "<==" { EXPLIES (emit_segment lexbuf (Some "<==" )) }
| "::" { DOUBLECOLON }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '['  { LBRACK }
| ']' { RBRACK }
| '.' { DOT }
| ':' { COLON }
| ';' { SEMICOLON }
| ',' { COMMA }
| "old" { OLD (emit_segment lexbuf (Some "old")) }
| "len" { LEN (emit_segment lexbuf (Some "len")) } 
| "max" { IDENTIFIER (emit_segment lexbuf (Some "maxListInt")) } 
| "filter" { IDENTIFIER (emit_segment lexbuf (Some "filterF")) }
| "map" { IDENTIFIER (emit_segment lexbuf (Some "mapF")) }
| "->" { ARROW }
| "->" { ARROW }
| "def" { DEF (emit_segment lexbuf (Some "def" )) }
| "lambda" { LAMBDA (emit_segment lexbuf (Some "lambda" )) }
| "if" { IF (emit_segment lexbuf (Some "if" )) }
| "elif" { ELIF (emit_segment lexbuf (Some "elif" )) }
| "else" { ELSE (emit_segment lexbuf (Some "else" )) }
| "for" { FOR (emit_segment lexbuf (Some "for" )) }
| "while" { WHILE (emit_segment lexbuf (Some "while" )) }
| "break" { BREAK (emit_segment lexbuf (Some "break" )) }
| "pass" { PASS (emit_segment lexbuf (Some "pass")) }
| "return" { RETURN (emit_segment lexbuf (Some "return")) }
| "assert" { ASSERT (emit_segment lexbuf (Some "assert")) }
| "not in" { NOT_IN (emit_segment lexbuf (Some "not in")) }
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
| "<=" { LTE (emit_segment lexbuf (Some "<=")) }
| '<' { LT (emit_segment lexbuf (Some "<")) }
| ">=" { GTE (emit_segment lexbuf (Some ">=")) }
| '>' { GT (emit_segment lexbuf (Some ">")) }
| "and" { AND (emit_segment lexbuf (Some "and")) }
| "or" { OR (emit_segment lexbuf (Some "or")) }
| "not" { NOT (emit_segment lexbuf (Some "not")) }
| "True" { TRUE }
| "False" { FALSE }
| "None" { NONE (emit_segment lexbuf (Some "None")) }
| float as f { FLOAT f }
| integer as i { INT i }
| identifier as i { IDENTIFIER (emit_segment lexbuf (Some i)) }
| strliteral as s { STRING (strip_quotes s) }
| whitespace { next_token lexbuf }
| comment { comment lexbuf }
| _ as c { illegal c }

and comment = parse
| indent as s { (next_line lexbuf (String.length s - 1); next_token lexbuf) } 
| _ { comment lexbuf }
