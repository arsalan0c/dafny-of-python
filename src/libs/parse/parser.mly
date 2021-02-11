/* --explain flag */
/* %left: reduce
%right: shift
%nonassoc: raise a SyntaxError */

%{
  open Astpy

  let rec replicate e n = match n with
    | 0 -> []
    | n -> [e]@(replicate e (n - 1))
%}

%token EOF INDENT DEDENT NEWLINE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COLON SEMICOLON COMMA TRUE FALSE NONE ARROW
%token <int> SPACE
%token <Sourcemap.segment> DEF IF ELSE FOR WHILE BREAK CONTINUE RETURN IN PRINT ASSERT
%token <Sourcemap.segment> AND OR NOT 
%token <Sourcemap.segment> IDENTIFIER TYPE
%token <string> STRING
%token <Sourcemap.segment> PLUS EQEQ EQ UMINUS NEQ LEQ LT GEQ GT PLUSEQ MINUS MINUSEQ TIMES TIMESEQ DIVIDE DIVIDEEQ MOD
%token <int> INT
%token PRE POST

%right EQ PLUSEQ MINUSEQ DIVIDEEQ TIMESEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left EQEQ NEQ
%left LT LEQ GT GEQ
%left OR
%left AND
%right NOT UMINUS
%left SEMICOLON

%nonassoc ELSE
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
  | s=stmt; NEWLINE { s }
  | s=stmt; SEMICOLON { s }
  | e=exp { Exp e }
  | s=spec; NEWLINE; DEF; i=id; LPAREN; fl=param_lst; RPAREN; ARROW; t=TYPE; COLON; sl=suite { Function (s, i, fl, Type t, sl) }
  | IF; e=exp; COLON; s1=suite; ELSE; COLON; s2=suite { IfElse (e, s1, s2) }
  | IF; e=exp; COLON; s=suite; { IfElse (e, s, []) }
  | RETURN; e=exp { Return e }
  | RETURN { Return (Literal (NoneLiteral)) }
  | WHILE; e=exp; COLON; s=suite; { While (e, s) }
  | CONTINUE { Continue }
  | BREAK { Break }
  | PRINT; LPAREN; e=exp RPAREN { Print e }
  | ASSERT; e=exp { Assert e }
  | al=assign_lst; EQ; e=exp; { Assign (al, replicate e (List.length al)) }
  /* | il=id_lst; EQ; el=exp_lst { Assign (il, el) } */
  | s1=IDENTIFIER; s2=PLUSEQ; e2=exp { Assign ([Identifier s1], [BinaryOp (Identifier s1, Plus s2, e2)]) }
  | s1=IDENTIFIER; s2=MINUSEQ; e2=exp { Assign ([Identifier s1], [BinaryOp (Identifier s1, Minus s2, e2)]) }
  | s1=IDENTIFIER; s2=TIMESEQ; e2=exp { Assign ([Identifier s1], [BinaryOp (Identifier s1, Times s2, e2)]) }
  | s1=IDENTIFIER; s2=DIVIDEEQ; e2=exp { Assign ([Identifier s1], [BinaryOp (Identifier s1, Divide s2, e2)]) }
  ;

assign_lst:
  | i=id { [i] }
  | al=assign_lst; EQ; i=id { al@[i] }
  ;

exp:
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
  | LPAREN; e=exp; RPAREN; { e }
  | s=MINUS; e=exp %prec UMINUS { UnaryOp (UMinus s, e) }
  | s=NOT; e=exp %prec NOT { UnaryOp (Not s, e) }
  | TRUE { Literal (BooleanLiteral true) }
  | FALSE { Literal (BooleanLiteral false) }
  | i=INT { Literal (IntegerLiteral (i)) }
  | s=STRING { Literal (StringLiteral (s)) }
  | NONE { Literal (NoneLiteral) }
  | s=IDENTIFIER { Identifier s }
  | e=exp; LPAREN; el=exp_lst; RPAREN { Call (e, el) }
  ;

spec:
  | PRE; pre=exp; NEWLINE; POST; post=exp; { Spec (pre, post) }

suite:
  | NEWLINE; INDENT; sl=stmts; DEDENT { sl }
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
  | i=IDENTIFIER; COLON; t=TYPE { Param (Identifier i, Type t) }
  ;

id:
  | i=IDENTIFIER { Identifier i }
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
