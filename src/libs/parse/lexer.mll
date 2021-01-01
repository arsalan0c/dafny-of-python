{
  open Parser

  exception LexError of string

  let[@inline] failwith msg = raise (LexError msg)

  let[@inline] illegal c =
    failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)

  let strip_quotes str =
    match String.length str with
    | 0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)
  
  let emit_segment lb v = ((Lexing.lexeme_start_p lb), v)
}

let indent = '\n' ' '*
let whitespace = [' ' '\t']

let identifier = ['a'-'z' 'A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let digit = ['0'-'9']
let integer = (digit | ['1' - '9'] digit*)
let stringliteral = ('"'[^'"''\\']*('\\'_[^'"''\\']*)*'"')
let comment = '#'
let boolean = "True" | "False"
let e = ""

rule f = parse
| eof { EOF }
| indent as s { SPACE (String.length s - 1) }
| whitespace+ { f lexbuf }
| "# pre" { PRE }
| "# post" { POST }
| "#pre" { PRE }
| "#post" { POST }
| comment { comment lexbuf }
| eof { EOF }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '['  { LBRACK }
| ']' { RBRACK }
| ':' { COLON }
| ';' { SEMICOLON }
| ',' { COMMA }
| "def" { DEF (emit_segment lexbuf None) }
| "if" { IF (emit_segment lexbuf None) }
| "else" { ELSE (emit_segment lexbuf None) }
| "for" { FOR (emit_segment lexbuf None) }
| "while" { WHILE (emit_segment lexbuf None) }
| "break" { BREAK (emit_segment lexbuf None) }
| "continue" { CONTINUE (emit_segment lexbuf None) }
| "print" { PRINT (emit_segment lexbuf None) }
| "return" { RETURN (emit_segment lexbuf None) }
| "assert" { ASSERT (emit_segment lexbuf None) }
| "in" { IN (emit_segment lexbuf None) }
| "==" { EQEQ (emit_segment lexbuf None) }
| '=' { EQ (emit_segment lexbuf None) }
| "!=" { NEQ (emit_segment lexbuf None) }
| '+' { PLUS (emit_segment lexbuf None) }
| "+=" { PLUSEQ (emit_segment lexbuf None) }
| '-' { MINUS (emit_segment lexbuf None) }
| "-=" { MINUSEQ (emit_segment lexbuf None) }
| '*' { TIMES (emit_segment lexbuf None) }
| "*=" { TIMESEQ (emit_segment lexbuf None) }
| "/" { DIVIDE (emit_segment lexbuf None) }
| "/=" { DIVIDEEQ (emit_segment lexbuf None) }
| "%" { MOD (emit_segment lexbuf None) }
| "<=" { LEQ (emit_segment lexbuf None) }
| '<' { LT (emit_segment lexbuf None) }
| ">=" { GEQ (emit_segment lexbuf None) }
| '>' { GT (emit_segment lexbuf None) }
| "and" { AND (emit_segment lexbuf None) }
| "or" { OR (emit_segment lexbuf None) }
| "not" { NOT (emit_segment lexbuf None) }
| "True" { TRUE }
| "False" { FALSE }
| "None" { NONE }
| integer as i { INT (int_of_string i) }
| identifier as i { IDENTIFIER (emit_segment lexbuf (Some i)) }
| stringliteral as s { STRING (strip_quotes s) }
| _ as c { illegal c }

and comment = parse
| indent { f lexbuf }
| _ { comment lexbuf }





