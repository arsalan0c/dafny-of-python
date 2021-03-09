/* --explain flag */
/* %left: reduce
%right: shift
menhir --list-errors
%nonassoc: raise a SyntaxError */

%{
  open Astpy

  let rec replicate e n = match n with
    | 0 -> []
    | n -> [e]@(replicate e ( n - 1))
%}

%token EOF INDENT DEDENT NEWLINE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COLON SEMICOLON COMMA TRUE FALSE NONE ARROW
%token <int> SPACE
%token <Sourcemap.segment> DEF IF ELIF ELSE FOR WHILE BREAK CONTINUE RETURN NOT_IN IN PRINT ASSERT LAMBDA PASS
%token <Sourcemap.segment> AND OR NOT 
%token <Sourcemap.segment> IDENTIFIER INT_TYPE FLOAT_TYPE BOOL_TYPE COMPLEX_TYPE STRING_TYPE NONE_TYPE LIST_TYPE DICT_TYPE SET_TYPE TUPLE_TYPE
%token <string> STRING
%token <Sourcemap.segment> IMPLIES EXPLIES BIIMPL PLUS EQEQ EQ UMINUS NEQ LEQ LT GEQ GT PLUSEQ MINUS MINUSEQ TIMES TIMESEQ DIVIDE DIVIDEEQ MOD
%token <int> INT
%token PRE POST INVARIANT FORALL EXISTS DECREASES DOUBLECOLON
%token LEN FILTER MAP


%left OR
%left AND
%left IMPLIES EXPLIES BIIMPL
%left EQEQ NEQ
%left LT LEQ GT GEQ
%right EQ PLUSEQ MINUSEQ DIVIDEEQ TIMESEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT UMINUS
%left SEMICOLON

%nonassoc ELSE ELIF
%nonassoc LPAREN LBRACK LBRACE
%nonassoc RPAREN RBRACK RBRACE

%start <sexp> f

/*  */
%%

f:
  | sl=stmts; EOF { Program sl }
  ;

stmts:
  | sl=list(stmt) { sl }
  ;

stmt:
  | s=stmt; NEWLINE { s }
  | s=stmt; SEMICOLON { s }
  | e=exp { Exp e }
  | s=list(spec); DEF; id=IDENTIFIER; LPAREN; fl=param_lst; RPAREN; ARROW; t=typ; COLON; sl=suite { Function (s, id, fl, t, sl) }
  | IF; e=exp; COLON; s1=suite; el=elif_lst; ELSE; COLON; s2=suite { IfElse (e, s1, el, s2) }
  | IF; e=exp; COLON; s=suite; el=elif_lst; { IfElse (e, s, el, []) }
  | RETURN; el=exp_lst { Return el }
  | RETURN { Return [Literal (NoneLiteral)] }
  | sl=list(spec); WHILE; e=exp; COLON; s=suite; { While (e, sl, s) }
  | CONTINUE { Continue }
  | BREAK { Break }
  | PASS { Pass }
  | PRINT; LPAREN; e=exp RPAREN { Print e }
  | ASSERT; e=exp { Assert e }
  | al=assign_lst; EQ; e=exp; { Assign (al, replicate e (List.length al)) }
  /* | il=id_lst; EQ; el=exp_lst { Assign (il, el) } */
  | s1=IDENTIFIER; s2=PLUSEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Plus s2, e2)]) }
  | s1=IDENTIFIER; s2=MINUSEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Minus s2, e2)]) }
  | s1=IDENTIFIER; s2=TIMESEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Times s2, e2)]) }
  | s1=IDENTIFIER; s2=DIVIDEEQ; e2=exp { Assign ([s1], [BinaryOp (Identifier s1, Divide s2, e2)]) }
  ;

elif_lst:
  | ELIF; e=exp; COLON; sl=suite; esl=elif_lst { (e, sl)::esl }
  | { [] }

assign_lst:
  | id=IDENTIFIER { [id] }
  | al=assign_lst; EQ; id=IDENTIFIER { al@[id] }
  ;
  
exp:
  | LPAREN; e=exp; RPAREN; { e }
  | el=lst_exp { el }
  | e=exp; s=slice { Subscript (e, s) }
  | s=MINUS; e=exp %prec UMINUS { UnaryOp (UMinus s, e) }
  | s=NOT; e=exp %prec NOT { UnaryOp (Not s, e) }
  | TRUE { Literal (BooleanLiteral true) }
  | FALSE { Literal (BooleanLiteral false) }
  | i=INT { Literal (IntegerLiteral (i)) }
  | s=STRING { Literal (StringLiteral (s)) }
  | NONE { Literal (NoneLiteral) }
  | s=IDENTIFIER; LPAREN; el=exp_lst; RPAREN { Call (s, el) }
  | s=IDENTIFIER { Identifier s }
  /* | LAMBDA; fl=param_lst; ARROW; t=typ; COLON; e=exp { }  */
  | LEN; LPAREN; e=exp; RPAREN; { Len e } 
  | e1=exp; seg=NOT_IN; e2=exp { BinaryOp (e1, NotIn seg, e2) }
  | e1=exp; seg=IN; e2=exp { BinaryOp (e1, In seg, e2) }
  | e1=exp; seg=PLUS; e2=exp { BinaryOp (e1, Plus seg, e2) }
  | e1=exp; seg=MINUS; e2=exp { BinaryOp (e1, Minus seg, e2) }
  | e1=exp; seg=TIMES; e2=exp { BinaryOp (e1, Times seg, e2) }
  | e1=exp; seg=DIVIDE; e2=exp { BinaryOp (e1, Divide seg, e2) }
  | e1=exp; seg=MOD; e2=exp { BinaryOp (e1, Mod seg, e2) }
  | e1=exp; seg=EQEQ; e2=exp { BinaryOp (e1, EqEq seg, e2) }
  | e1=exp; seg=NEQ; e2=exp { BinaryOp (e1, NEq seg, e2) }
  | e1=exp; seg=LT; e2=exp { BinaryOp (e1, Lt seg, e2) }
  | e1=exp; seg=LEQ; e2=exp { BinaryOp (e1, LEq seg, e2) }
  | e1=exp; seg=GT; e2=exp { BinaryOp (e1, Gt seg, e2) }
  | e1=exp; seg=GEQ; e2=exp { BinaryOp (e1, GEq seg, e2) }
  | e1=exp; seg=AND; e2=exp { BinaryOp (e1, And seg, e2) }
  | e1=exp; seg=OR; e2=exp { BinaryOp (e1, Or seg, e2) }
  | e1=exp; seg=BIIMPL; e2=exp { BinaryOp (e1, BiImpl seg, e2) }
  | e1=exp; seg=IMPLIES; e2=exp { BinaryOp (e1, Implies seg, e2) }
  | e1=exp; seg=EXPLIES; e2=exp { BinaryOp (e1, Explies seg, e2) }
  | FORALL; il=id_lst; DOUBLECOLON; e=exp; { Forall (il, e) }
  | EXISTS; il=id_lst; DOUBLECOLON; e=exp; { Exists (il, e) }
  ;

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

/* spec_prepost:
  | PRE; pre=exp; NEWLINE; POST; post=exp; { [(Pre pre), (Post post)] } */

suite:
  | NEWLINE; INDENT; sl=stmts; DEDENT { sl }
  ;

/* exp_lst:
  | { [] }
  | el=exp_rest; { el }
exp_rest:
  | e=exp; { [e] }
  | er=exp_rest; COMMA; 3e=exp { er@[e] } */

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
  | t=COMPLEX_TYPE { Complex t }
  | t=BOOL_TYPE { Bool t }
  | t=STRING_TYPE { Str t }
  | t=NONE_TYPE { Non t }
  ;

data_typ:
  | l=LIST_TYPE LBRACK t=typ RBRACK { List(l, Some t) }
  | l=LIST_TYPE { List(l, None) }
  | d=DICT_TYPE LBRACK t=typ RBRACK { Dict(d, Some t) }
  | d=DICT_TYPE { Dict(d, None) }
  | s=SET_TYPE LBRACK t=typ RBRACK { Set(s, Some t) }
  | s=SET_TYPE { Set(s, None) }
  | tt=TUPLE_TYPE LBRACK tl=typ_lst RBRACK { Tuple(tt, Some tl) }
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
