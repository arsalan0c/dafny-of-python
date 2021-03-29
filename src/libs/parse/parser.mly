/* --explain flag */
/* %left: reduce
%right: shift
menhir --list-errors
%nonassoc: raise a SyntaxError */

%{
  open Astpy
  let printf = Stdlib.Printf.printf
%}

%token EOF INDENT DEDENT NEWLINE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK DOT COLON SEMICOLON COMMA TRUE FALSE NONE ARROW
%token <int> SPACE
%token <Sourcemap.segment> DEF IF ELIF ELSE WHILE FOR BREAK RETURN NOT_IN IN ASSERT LAMBDA PASS
%token <Sourcemap.segment> AND OR NOT 
%token <Sourcemap.segment> IDENTIFIER INT_TYP FLOAT_TYP BOOL_TYP STRING_TYP NONE_TYP LIST_TYP DICT_TYP SET_TYP TUPLE_TYP CALLABLE_TYP UNION_TYP
%token <string> STRING
%token <Sourcemap.segment> IMPLIES EXPLIES BIIMPL PLUS EQEQ EQ NEQ LTE LT GTE GT PLUSEQ MINUS MINUSEQ TIMES TIMESEQ DIVIDE DIVIDEEQ MOD
%token <int> INT
%token <float> FLOAT
%token PRE POST INVARIANT FORALL EXISTS DECREASES READS MODIFIES DOUBLECOLON 
%token <Sourcemap.segment> LEN OLD FRESH 

%left BIIMPL IMPLIES EXPLIES 
%left OR 
%left AND
%left EQEQ NEQ
%left LT LTE GT GTE
%right EQ PLUSEQ MINUSEQ DIVIDEEQ TIMESEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%left SEMICOLON 
%left COMMA

%nonassoc ELSE ELIF
%nonassoc LPAREN LBRACK LBRACE
%nonassoc RPAREN RBRACK RBRACE

%start <program> program

%%

program:
  | sl=stmts_plus; EOF { Program sl }
  ;

stmts_plus:
  | { [] }
  | s=stmt { [s] }
  | s=stmt; NEWLINE; sl=stmts_plus { s::sl }
  | s=stmt; SEMICOLON; sl=stmts_plus { s::sl }
  ;

newline_star:
  | NEWLINE; newline_star { [] }
  | { [] }
  ;

stmt:
  | newline_star; s=compound_stmt { s }
  | newline_star; s=small_stmt { s }
  ;

small_stmt:
  | a=assignment { a }
  | e=star_exps { Exp e }
  | RETURN; el=star_exps { Return el }
  | PASS { Pass }
  | ASSERT; e=exp { Assert e }
  | BREAK { Break }
  ;

compound_stmt:
  | specl=list(spec); DEF; id=IDENTIFIER; LPAREN; fl=param_star; RPAREN; ARROW; t=exp; COLON; sl=block { Function (specl, id, fl, t, sl) }
  | IF; e=exp; COLON; s1=block; el=elif_star; ELSE; COLON; s2=block { IfElse (e, s1, el, s2) }
  | IF; e=exp; COLON; s=block; el=elif_star; { IfElse (e, s, el, []) }
  | specl=list(spec); FOR; t=star_targets; IN; e=star_exps; COLON; b=block { For (specl, t, e, b) }
  | specl=list(spec); WHILE; e=exp; COLON; s=block; { While (specl, e, s) }
  ;

assignment:
  | id=IDENTIFIER; COLON; t=exp; EQ; e2=star_exps { Assign (Some t, [Identifier id], [e2]) }
  | id=IDENTIFIER; EQ; e2=star_exps { Assign (None, [Identifier id], [e2]) } (* used for type aliasing and variable updates *)
  | s1=IDENTIFIER; s2=PLUSEQ; e2=star_exps { Assign (None, [Identifier s1], [BinaryExp (Identifier s1, Plus s2, e2)]) }
  | s1=IDENTIFIER; s2=MINUSEQ; e2=star_exps { Assign (None, [Identifier s1], [BinaryExp (Identifier s1, Minus s2, e2)]) }
  | s1=IDENTIFIER; s2=TIMESEQ; e2=star_exps { Assign (None, [Identifier s1], [BinaryExp (Identifier s1, Times s2, e2)]) }
  | s1=IDENTIFIER; s2=DIVIDEEQ; e2=star_exps { Assign (None, [Identifier s1], [BinaryExp (Identifier s1, Divide s2, e2)]) }
  ;

elif_star:
  | ELIF; e=exp; COLON; sl=block; esl=elif_star { (e, sl)::esl }
  | { [] }
  ;

star_exps:
  | e=exp; el=star_exps_rest; COMMA { Tuple (e::el) }
  | e=exp; el=star_exps_rest { Tuple (e::el) }
  | e=exp; COMMA { Tuple [e] }
  | e=exp { e }
  ;

star_exps_rest:
  | el=star_exps_rest; COMMA; e=exp;  { el@[e] }
  | COMMA; e=exp { [e] }
  ;

star_targets:
  | st=star_target; str=star_targets_rest; COMMA; { [st]@str }
  | st=star_target; str=star_targets_rest { [st]@str }
  | st=star_target { [st] }
  ;

star_targets_rest:
  | str=star_targets_rest; COMMA; st=star_target; { str@[st] }
  | COMMA; st=star_target; { [st] }
  ;

star_target:
  | id=IDENTIFIER { Identifier id }
  | LPAREN st=star_targets RPAREN { Tuple st } 
  | LBRACK st=star_targets RBRACK { Lst st } 
  ;

exp:
  | d1=implication; IF; d2=implication; ELSE; im=exp { IfElseExp (d1, d2, im) }
  | LAMBDA; il=id_star; COLON; e=exp { Lambda (il, e) }
  | e=implication { e }
  | e=typ { Typ e }
  ;

implication:
  | bim=implication; s=BIIMPL; d=disjunction { BinaryExp (bim, BiImpl s, d) }
  | im=implication; s=IMPLIES; d=disjunction { BinaryExp (im, Implies s, d) }
  | im=implication; s=EXPLIES; d=disjunction { BinaryExp (im, Explies s, d) }
  | d=disjunction; { d }
  ;

disjunction:
  | d=disjunction; s=OR; c=conjunction { BinaryExp (d, Or s, c) }
  | c=conjunction { c }
  ;

conjunction:
  | ir=conjunction; s=AND; i=inversion { BinaryExp (ir, And s, i) }
  | i=inversion { i }
  ;

inversion:
  | s=NOT; i=inversion { UnaryExp (Not s, i) }
  | c=comparison { c }
  ;

comparison:
  | c=comparison; s=EQEQ; e=sum { BinaryExp (c, EqEq s, e) }
  | c=comparison; s=NEQ; e=sum { BinaryExp (c, NEq s, e) }
  | c=comparison; s=LTE; e=sum { BinaryExp (c, LEq s, e) }
  | c=comparison; s=LT; e=sum { BinaryExp (c, Lt s, e) }
  | c=comparison; s=GTE; e=sum { BinaryExp (c, GEq s, e) }
  | c=comparison; s=GT; e=sum { BinaryExp (c, Gt s, e) }
  | c=comparison; s=NOT_IN; e=sum { BinaryExp (c, NotIn s, e) }
  | c=comparison; s=IN; e=sum { BinaryExp (c, In s, e) }
  | s=sum { s }
  ;

sum:
  | e=sum; s=PLUS; t=term { BinaryExp (e, Plus s, t) }
  | e=sum; s=MINUS; t=term { BinaryExp (e, Minus s, t) }
  | t=term { t }
  ; 

term:
  | t=term; s=TIMES; f=factor { BinaryExp (t, Times s, f) }
  | t=term; s=DIVIDE; f=factor { BinaryExp (t, Divide s, f) }
  | t=term; s=MOD; f=factor { BinaryExp (t, Mod s, f) }
  | f=factor { f }
  ;

factor:
  | PLUS; f=factor { f }
  | s=MINUS; f=factor { UnaryExp (UMinus s, f) }
  | p=power { p }
  ;

power:
  | p=primary { p }
  ;

primary:
  | e=primary DOT s=IDENTIFIER { Dot (e, s) }
  | e=primary LPAREN el=arguments RPAREN { Call (e, el) }
  | e=primary s=slice { Subscript (e, s) }
  | a=atom { a }
  ;

arguments:
  |  el=exp_star { el } (* TODO: allow default arguments *)
  ;

atom:
  | s=IDENTIFIER { Identifier s }
  | TRUE { Literal (BoolLit true) }
  | FALSE { Literal (BoolLit false) }
  | i=INT { Literal (IntLit i) }
  | i=FLOAT { Literal (FloatLit i) }
  | s=strings { Literal (StringLit s) }
  | NONE { Literal (NonLit) }
  | FORALL; il=id_star; DOUBLECOLON; e=exp { Forall (il, e) }
  | EXISTS; il=id_star; DOUBLECOLON; e=exp { Exists (il, e) }
  | LPAREN; e=exp; COMMA; el=exp_star; RPAREN { Tuple (e::el) }
  | LPAREN; e=exp; RPAREN; { e }
  | l=lst_exp { l }
  | s=set_exp { s }
  | d=dict_exp { d }
  | s=LEN; LPAREN; e=star_exps; RPAREN; { Len (s, e) }
  | s=OLD; LPAREN; e=star_exps; RPAREN; { Old (s, e) }
  | s=FRESH; LPAREN; e=star_exps; RPAREN; { Fresh (s, e) }
  (* TODO: add comprehensions *)
  ;

strings:
  | s=STRING { s }
  | sl=strings; s=STRING { sl ^ s }
  ;

slice: 
  | LBRACK; e1=exp; COLON; e2=exp; RBRACK { Slice (Some e1, Some e2) } 
  | LBRACK; e=exp; COLON; RBRACK { Slice (Some e, None) } 
  | LBRACK; COLON; e=exp  RBRACK { Slice (None, Some e) }
  | LBRACK; COLON; RBRACK { Slice (None, None) }  
  | LBRACK; e=exp; RBRACK { Index e }
  ; 

lst_exp:
  | LBRACK; el=exp_star; RBRACK { Lst el }
  ;

dict_exp:
  | LBRACE; eel=kv_star; RBRACE { Dict eel }
  ;

kv_star:
  | pr=kv_rest; { pr }
  | { [] }
  ;

kv_rest:
  | p=kv COMMA; pr=kv_rest { p::pr }
  | p=kv; COMMA { [p] }
  | p=kv; { [p] }
  ;

kv:
  | k=exp; COLON; v=exp { (k, v) }
  ;

set_exp:
  | LBRACE; el=exp_star; RBRACE { Set el }
  ;

spec:
  | PRE; e=spec_rem { Pre e }
  | POST; e=spec_rem { Post e }
  | DECREASES; e=spec_rem { Decreases e }
  | INVARIANT; e=spec_rem { Invariant e }
  | READS; e=spec_rem { Reads e }
  | MODIFIES; e=spec_rem { Modifies e }
  ;

spec_rem:
  | e=exp; NEWLINE { e }
  ;

block:
  | NEWLINE; newline_star; ni=indent_plus; sl=stmts_plus; nd=dedent_plus 
  { let diff = ni - nd in printf "Diff: %d\n" diff; if (diff != 0) then failwith "unexpected indentation" else sl }
  ;

indent_plus:
  | INDENT { 1 }
  /* | INDENT; n=indent_plus;  { n + 1 } */
  ;

dedent_plus:
  | DEDENT { 1 }
  /* | DEDENT; n=dedent_plus;  { n + 1 } */
  ;

typ_id:
  | t=typ { t }
  | t=IDENTIFIER { TIdent t }
  ;

typ:
  | dt=data_typ { dt }
  | t=base_typ { t }
  ;

typ_plus:
  | tr=typ_plus; COMMA; t=typ_id  { tr@[t] }
  | t=typ_id { [t] }
  ;

base_typ:
  | t=STRING_TYP { TStr t }
  | t=INT_TYP { TInt t }
  | t=FLOAT_TYP { TFloat t }
  | t=BOOL_TYP { TBool t }
  | t=NONE_TYP { TNone t }
  ;

data_typ:
  | l=LIST_TYP LBRACK t=typ_id RBRACK { TLst (l, Some t) }
  | l=LIST_TYP { TLst (l, None) }
  | d=DICT_TYP LBRACK t1=typ_id COMMA t2=typ_id RBRACK { TDict (d, Some t1, Some t2) }
  | d=DICT_TYP { TDict (d, None, None) }
  | s=SET_TYP LBRACK t=typ_id RBRACK { TSet (s, Some t) }
  | s=SET_TYP { TSet (s, None) }
  | tt=TUPLE_TYP LBRACK tl=typ_plus; RBRACK { TTuple (tt, Some tl) }
  | t=TUPLE_TYP { TTuple (t, None) }
  | ct=CALLABLE_TYP LBRACK LBRACK tl=typ_plus RBRACK COMMA t=typ_id RBRACK { TCallable (ct, tl, t)  }
  /* | u=UNION_TYP LBRACK tl=typ_plus; RBRACK { Union (u, tl) } */
  ;

param_star:
  | pr=param_rest; { pr }
  | { [] }
  ;

param_rest:
  | p=param; COMMA; pr=param_rest { p::pr }
  | p=param; COMMA { [p] }
  | p=param;  { [p] }
  ;

param:
  | id=IDENTIFIER; COLON; t=exp { Param (id, t) }
  ;

id_star:
  | { [] }
  | ir=id_rest { ir }
  ;

id_rest:
  | id=IDENTIFIER; COMMA; ir=id_rest { id::ir }
  | id=IDENTIFIER { [id] }
  ;

exp_star:
  | er=exp_rest { er }
  | { [] }
  ;

exp_rest:
  | e=exp; COMMA; er=exp_rest;   { e::er }
  | e=exp; COMMA { [e] }
  | e=exp { [e] }
  ;

%%
