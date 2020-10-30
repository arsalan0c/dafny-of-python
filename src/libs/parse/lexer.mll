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
| "def" { DEF }
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "break" { BREAK }
| "continue" { CONTINUE }
| "print" { PRINT }
| "return" { RETURN }
| "assert" { ASSERT }
| "in" { IN }
| "==" { EQEQ }
| '=' { EQ }
| "!=" { NEQ }
| '+' { PLUS }
| "+=" { PLUSEQ }
| '-' { MINUS }
| "-=" { MINUSEQ }
| '*' { TIMES }
| "*=" { TIMESEQ }
| "/" { DIVIDE }
| "/=" { DIVIDEEQ }
| "%" { MOD }
| "<=" { LEQ }
| '<' { LT }
| ">=" { GEQ }
| '>' { GT }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| "True" { TRUE }
| "False" { FALSE }
| "None" { NONE }
| integer as i { INT (int_of_string i) }
| identifier as i { IDENTIFIER i }
| stringliteral as s { STRING (strip_quotes s) }
| _ as c { illegal c }

and comment = parse
| indent { f lexbuf }
| _ { comment lexbuf }


