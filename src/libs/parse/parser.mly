/* --explain flag */
/* %left: reduce
%right: shift
menhir --list-errors
%nonassoc: raise a SyntaxError */

%{
  open Astpy
  let printf = Stdlib.Printf.printf
%}

%token EOF INDENT DEDENT NEWLINE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COLON SEMICOLON COMMA TRUE FALSE NONE ARROW
%token <int> SPACE
%token <Sourcemap.segment> DEF IF ELIF ELSE WHILE BREAK RETURN NOT_IN IN ASSERT LAMBDA PASS
%token <Sourcemap.segment> AND OR NOT 
%token <Sourcemap.segment> IDENTIFIER INT_TYP FLOAT_TYP BOOL_TYP STRING_TYP NONE_TYP LIST_TYP DICT_TYP SET_TYP TUPLE_TYP
%token <string> STRING
%token <Sourcemap.segment> IMPLIES EXPLIES BIIMPL PLUS EQEQ EQ NEQ LTE LT GTE GT PLUSEQ MINUS MINUSEQ TIMES TIMESEQ DIVIDE DIVIDEEQ MOD
%token <int> INT
%token PRE POST INVARIANT FORALL EXISTS DECREASES DOUBLECOLON
%token LEN

%left OR
%left AND
%left IMPLIES EXPLIES BIIMPL
%left EQEQ NEQ
%left LT LTE GT GTE
%right EQ PLUSEQ MINUSEQ DIVIDEEQ TIMESEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%left SEMICOLON

%nonassoc ELSE ELIF
%nonassoc LPAREN LBRACK LBRACE
%nonassoc RPAREN RBRACK RBRACE

%start <program> f

%%

f:
  | sl=stmts_plus; EOF { Program sl }
  ;

stmts_plus:
  | { [] }
  | s=stmt { [s] }
  | s=stmt; NEWLINE; sl=stmts_plus { s::sl }
  | s=stmt; SEMICOLON; sl=stmts_plus { s::sl }
  ;

newline_star:
  | { [] }
  | NEWLINE; newline_star { [] }
  ;

stmt:
  | newline_star; s=compound_stmt { s }
  | newline_star; s=small_stmt { s }
  ;

small_stmt:
  | a=assignment { a }
  | e=exp { Exp e }
  | RETURN; el=exp_star { if List.length el = 0 then Return [Literal (NoneLiteral)] else Return el }
  | PASS { Pass }
  | ASSERT; e=exp { Assert e }
  | BREAK { Break }
  ;

compound_stmt:
  | s=list(spec); DEF; id=IDENTIFIER; LPAREN; fl=param_star; RPAREN; ARROW; t=typ; COLON; sl=block { Function (s, id, fl, t, sl) }
  | IF; e=exp; COLON; s1=block; el=elif_star; ELSE; COLON; s2=block { IfElse (e, s1, el, s2) }
  | IF; e=exp; COLON; s=block; el=elif_star; { IfElse (e, s, el, []) }
  | sl=list(spec); WHILE; e=exp; COLON; s=block; { While (e, sl, s) }
  ;

assignment:
  | id=IDENTIFIER; COLON; typ; EQ; e2=exp { Assign ([id], [e2])}  /* TODO: make use of the identifier type */
  | id=IDENTIFIER; EQ; e2=typ { Assign ([id], [Typ e2]) }
  | s1=IDENTIFIER; s2=PLUSEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Plus s2, e2)]) }
  | s1=IDENTIFIER; s2=MINUSEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Minus s2, e2)]) }
  | s1=IDENTIFIER; s2=TIMESEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Times s2, e2)]) }
  | s1=IDENTIFIER; s2=DIVIDEEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Divide s2, e2)]) }
  ;

elif_star:
  | ELIF; e=exp; COLON; sl=block; esl=elif_star { (e, sl)::esl }
  | { [] }
  ;

exp:
  | d=implication; { d }
  | FORALL; il=id_star; DOUBLECOLON; e=implication { Forall (il, e) }
  | EXISTS; il=id_star; DOUBLECOLON; e=implication { Exists (il, e) }
  | LAMBDA; param_star; ARROW; typ; COLON; e=implication { e }
  ;

implication:
  | d=disjunction; { d }
  | im=implication; s=BIIMPL; d=disjunction { BinaryOp (im, BiImpl s, d) }
  | im=implication; s=IMPLIES; d=disjunction { BinaryOp (im, Implies s, d) }
  | im=implication; s=EXPLIES; d=disjunction { BinaryOp (im, Explies s, d) }
  ;

disjunction:
  | c=conjunction { c }
  | d=disjunction; s=OR; c=conjunction { BinaryOp (d, Or s, c) }
  ;

conjunction:
  | i=inversion { i }
  | ir=conjunction; s=AND; i=inversion { BinaryOp (ir, And s, i) }
  ;

inversion:
  | s=NOT; i=inversion { UnaryOp(Not s, i) }
  | c=comparison { c }
  ;

comparison:
  | s=sum { s }
  | c=comparison; s=EQEQ; e=sum { BinaryOp (c, EqEq s, e) }
  | c=comparison; s=NEQ; e=sum { BinaryOp (c, NEq s, e) }
  | c=comparison; s=LTE; e=sum { BinaryOp (c, LEq s, e) }
  | c=comparison; s=LT; e=sum { BinaryOp (c, Lt s, e) }
  | c=comparison; s=GTE; e=sum { BinaryOp (c, GEq s, e) }
  | c=comparison; s=GT; e=sum { BinaryOp (c, Gt s, e) }
  | c=comparison; s=NOT_IN; e=sum { BinaryOp (c, NotIn s, e) }
  | c=comparison; s=IN; e=sum { BinaryOp (c, In s, e) }
  ;

sum:
  | e=sum; s=PLUS; t=term { BinaryOp(e, Plus s, t) }
  | e=sum; s=MINUS; t=term { BinaryOp(e, Minus s, t) }
  | t=term { t }
  ; 

term:
  | t=term; s=TIMES; f=factor { BinaryOp (t, Times s, f) }
  | t=term; s=DIVIDE; f=factor { BinaryOp (t, Divide s, f) }
  | t=term; s=MOD; f=factor { BinaryOp (t, Mod s, f) }
  | f=factor { f }
  ;

factor:
  | PLUS; f=factor { f }
  | s=MINUS; f=factor { UnaryOp(UMinus s, f) }
  | p=power { p }
  ;

power:
  | p=primary { p }
  ;

primary:
  | s=IDENTIFIER; el=arguments { Call (s, el) } (* TODO: allow primaries as calls *)
  | e=primary; s=slice { Subscript (e, s) }
  | a=atom { a }
  ;

arguments:
  | LPAREN; el=exp_star; RPAREN { el } (* TODO: allow default arguments *)
  ;

atom:
  | s=IDENTIFIER { Identifier s }
  | TRUE { Literal (BooleanLiteral true) }
  | FALSE { Literal (BooleanLiteral false) }
  | s=strings { Literal (StringLiteral s) }
  | NONE { Literal (NoneLiteral) }
  | i=INT { Literal (IntegerLiteral (i)) }
  | LPAREN; e=exp; COMMA; el=exp_star; RPAREN { Tuple (e::el) }
  | LPAREN; e=exp; RPAREN; { e }
  | el=lst_exp { el }
  | LEN; LPAREN; e=exp; RPAREN; { Len e }
  (* TODO: add slices, non-bracketed tuples and comprehensions *)
  ;

strings:
  | s=STRING { s }
  | sl=strings; s=STRING { sl ^ s }
  ;

slice: 
  | LBRACK; e=exp; o=slice_h { Slice (Some e, o) } 
  ; 
slice_h:
  | RBRACK { None }
  | COLON; e=exp; RBRACK { Some e }
  ;

lst_exp:
  | LBRACK; el=exp_star; RBRACK { Lst el }
  ;

spec:
  | PRE; e=spec_rem { Pre e }
  | POST; e=spec_rem { Post e }
  | DECREASES; e=spec_rem { Decreases e }
  | INVARIANT; e=spec_rem { Invariant e }
  ;

spec_rem:
  | e=exp; NEWLINE { e }
  ;

block:
  | NEWLINE; ni=indent_plus; sl=stmts_plus; nd=dedent_plus 
  { let diff = ni - nd in printf "Diff: %d\n" diff; if (diff != 0) then failwith "unexpected indentation" else sl }
  ;

indent_plus:
  | INDENT { 1 }
  | INDENT; n=indent_plus;  { n + 1 }
  ;

dedent_plus:
  | DEDENT { 1 }
  | DEDENT; n=dedent_plus;  { n + 1 }
  ;

typ:
  | t=base_typ { t }
  | dt=data_typ { dt }
  ;

typ_plus:
  | t=typ { [t] }
  | t=typ; COMMA; tr=typ_plus { t::tr }
  ;

base_typ:
  | t=INT_TYP { Int t }
  | t=FLOAT_TYP { Float t }
  | t=BOOL_TYP { Bool t }
  | t=STRING_TYP { Str t }
  | t=NONE_TYP { Non t }
  | t=IDENTIFIER { IdentTyp t }
  ;

data_typ:
  | l=LIST_TYP LBRACK t=typ RBRACK { LstTyp (l, Some t) }
  | l=LIST_TYP { LstTyp (l, None) }
  | d=DICT_TYP LBRACK t1=typ COMMA t2=typ RBRACK { Dict (d, Some t1, Some t2) }
  | d=DICT_TYP { Dict (d, None, None) }
  | s=SET_TYP LBRACK t=typ RBRACK { Set (s, Some t) }
  | s=SET_TYP { Set (s, None) }
  | tt=TUPLE_TYP; LBRACK; tl=typ_plus; RBRACK { Tuple (tt, Some tl) }
  | t=TUPLE_TYP { Tuple (t, None) }
  ;

param_star:
  | { [] }
  | pr=param_rest; { pr }
  ;

param_rest:
  | p=param;  { [p] }
  | p=param; COMMA; pr=param_rest { p::pr }
  ;

param:
  | id=IDENTIFIER; COLON; t=typ { Param (id, t) }
  ;

id_star:
  | { [] }
  | ir=id_rest { ir }
  ;

id_rest:
  | id=IDENTIFIER { [id] }
  | id=IDENTIFIER; COMMA; ir=id_rest { id::ir }
  ;

exp_star:
  | { [] }
  | er=exp_rest { er }
  ;

exp_rest:
  | e=exp { [e] }
  | e=exp; COMMA; er=exp_rest;   { e::er }
  ;

%%
