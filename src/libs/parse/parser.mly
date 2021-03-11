/* --explain flag */
/* %left: reduce
%right: shift
menhir --list-errors
%nonassoc: raise a SyntaxError */

%{
  open Astpy
%}

%token EOF INDENT DEDENT NEWLINE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COLON SEMICOLON COMMA TRUE FALSE NONE ARROW
%token <int> SPACE
%token <Sourcemap.segment> DEF IF ELIF ELSE FOR WHILE BREAK RETURN NOT_IN IN PRINT ASSERT LAMBDA PASS
%token <Sourcemap.segment> AND OR NOT 
%token <Sourcemap.segment> IDENTIFIER INT_TYPE FLOAT_TYPE BOOL_TYPE COMPLEX_TYPE STRING_TYPE NONE_TYPE LIST_TYPE DICT_TYPE SET_TYPE TUPLE_TYPE
%token <string> STRING
%token <Sourcemap.segment> IMPLIES EXPLIES BIIMPL PLUS EQEQ EQ NEQ LTE LT GTE GT PLUSEQ MINUS MINUSEQ TIMES TIMESEQ DIVIDE DIVIDEEQ MOD
%token <int> INT
%token PRE POST INVARIANT FORALL EXISTS DECREASES DOUBLECOLON
%token LEN FILTER MAP

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

%start <sexp> f

%%

f:
  | sl=stmts; EOF { Program sl }
  ;

stmts:
  | sl=list(stmt) { sl }
  ;

stmt:
  | s=compound_stmt { s }
  | s=simple_stmt { s }
  ;

simple_stmt:
  | s=small_stmt; NEWLINE { s }
  /* | s=small_stmt; SEMICOLON { s } */
  /* | s1=IDENTIFIER; s2=PLUSEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Plus s2, e2)]) }
  | s1=IDENTIFIER; s2=MINUSEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Minus s2, e2)]) }
  | s1=IDENTIFIER; s2=TIMESEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Times s2, e2)]) }
  | s1=IDENTIFIER; s2=DIVIDEEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Divide s2, e2)]) } */
  ;

small_stmt:
  | a=assignment { a }
  | e=exp { Exp e }
  | RETURN; el=exp_lst { Return el }
  | RETURN { Return [Literal (NoneLiteral)] }
  | PASS { Pass }
  | ASSERT; e=exp { Assert e }
  | BREAK { Break }
  ;

compound_stmt:
  | s=list(spec); DEF; id=IDENTIFIER; LPAREN; fl=param_lst; RPAREN; ARROW; t=typ; COLON; sl=suite { Function (s, id, fl, t, sl) }
  | IF; e=exp; COLON; s1=suite; el=elif_lst; ELSE; COLON; s2=suite { IfElse (e, s1, el, s2) }
  | IF; e=exp; COLON; s=suite; el=elif_lst; { IfElse (e, s, el, []) }
  | sl=list(spec); WHILE; e=exp; COLON; s=suite; { While (e, sl, s) }
  ;

assignment:
  | id=IDENTIFIER; COLON; exp; EQ; e2=exp { Assign ([id], [e2])}  /* TODO: use identifier type */
  ;
  /* | al=assign_lst; EQ; e=exp; { Assign (al, replicate e (List.length al)) } */
  /* | il=id_lst; EQ; el=exp_lst { Assign (il, el) } */  
/* assign_lst:
  | id=IDENTIFIER { [id] }
  | al=assign_lst; EQ; id=IDENTIFIER { al@[id] }
  ; */

elif_lst:
  | ELIF; e=exp; COLON; sl=suite; esl=elif_lst { (e, sl)::esl }
  | { [] }
  ;

exp:
  | d=disjunction; { d }
  /* | LAMBDA; param_lst; ARROW; typ; COLON; e=exp { e } */
  ;

disjunction:
  | c=conjunction { c }
  | d=disjunction; s=OR; c=conjunction { BinaryOp(d, Or s, c) }
  ;

conjunction:
  | i=inversion { i }
  | ir=conjunction; s=AND; i=inversion { BinaryOp(ir, And s, i) }
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
  /* | c=comparison; s=BIIMPL; e=sum { BinaryOp (c, BiImpl s, e) }
  | c=comparison; s=IMPLIES; e=sum { BinaryOp (c, Implies s, e) }
  | c=comparison; s=EXPLIES; e=sum { BinaryOp (c, Explies s, e) } */
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
  | s=IDENTIFIER; el=arguments { Call(s, el) } (* TODO: allow primaries as calls *)
  | e=primary; s=slice { Subscript (e, s) }
  | a=atom { a }
  ;

arguments:
  | LPAREN; el=exp_lst; RPAREN { el } (* TODO: allow default arguments *)
  ;

atom:
  | s=IDENTIFIER { Identifier s }
  | TRUE { Literal (BooleanLiteral true) }
  | FALSE { Literal (BooleanLiteral false) }
  | s=strings { Literal (StringLiteral s) }
  | NONE { Literal (NoneLiteral) }
  | i=INT { Literal (IntegerLiteral (i)) }
  | LPAREN; e=exp; COMMA; el=exp_lst; RPAREN { Tuple(e::el) }
  | LPAREN; e=exp; RPAREN; { e }
  | el=lst_exp { el }
  | LEN; LPAREN; e=exp; RPAREN; { Len e }
  /* | FORALL; il=id_lst; DOUBLECOLON; e=exp; { Forall (il, e) }
  | EXISTS; il=id_lst; DOUBLECOLON; e=exp; { Exists (il, e) } */
  /* | t=typ { Type t } */
  ;

strings:
  | s=STRING { s }
  | sl=strings; s=STRING { sl ^ s }

slice: 
  | LBRACK; e=exp; o=slice_h { Slice (Some(e), o) } 
  ; 
slice_h:
  | RBRACK { None }
  | COLON; e=exp; RBRACK { Some e }
  ;

lst_exp:
  | LBRACK; el=exp_lst; RBRACK { Lst el }
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

suite:
  | NEWLINE; INDENT; sl=stmts; DEDENT { sl }
  ;

typ:
  | t=base_typ { t }
  | dt=data_typ { dt }
  ;

typ_lst:
  | { [] }
  | tr=typ_rest; { tr }
  ;

typ_rest:
  | t=typ { [t] }
  | tr=typ_rest; COMMA; t=typ { tr@[t] }
  ;

base_typ:
  | t=INT_TYPE { Int t }
  | t=FLOAT_TYPE { Float t }
  | t=BOOL_TYPE { Bool t }
  | t=STRING_TYPE { Str t }
  | t=NONE_TYPE { Non t }
  | t=IDENTIFIER { IdentType t }
  ;

data_typ:
  | l=LIST_TYPE LBRACK t=typ RBRACK { List(l, Some t) }
  | l=LIST_TYPE { List(l, None) }
  | d=DICT_TYPE LBRACK t1=typ COMMA t2=typ RBRACK { Dict(d, Some t1, Some t2) }
  | d=DICT_TYPE { Dict(d, None, None) }
  | s=SET_TYPE LBRACK t=typ RBRACK { Set(s, Some t) }
  | s=SET_TYPE { Set(s, None) }
  | tt=TUPLE_TYPE; LBRACK; tl=typ_lst; RBRACK { Tuple(tt, Some tl) }
  | t=TUPLE_TYPE { Tuple(t, None) }
  ;

param_lst:
  | { [] }
  | pr=param_rest; { pr }
  ;

param_rest:
  | p=param;  { [p] }
  | pr=param_rest; COMMA; p=param { pr@[p] }
  ;

param:
  | id=IDENTIFIER; COLON; t=typ { Param (id, t) }
  ;

id_lst:
  | { [] }
  | ir=id_rest { ir }
  ;

id_rest:
  | id=IDENTIFIER { [id] }
  | ir=id_rest; COMMA; id=IDENTIFIER { ir@[id] }
  ;

exp_lst:
  | { [] }
  | er=exp_rest { er }
  ;

exp_rest:
  | e=exp { [e] }
  | er=exp_rest; COMMA; e=exp { er@[e] }
  ;

%%
