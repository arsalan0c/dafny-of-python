%{
  open Ast
%}

%token EOF INDENT DEDENT NEWLINE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COLON SEMICOLON COMMA
%token EQEQ EQ UMINUS NEQ LEQ LT GEQ GT PLUS PLUSEQ MINUS MINUSEQ TIMES TIMESEQ DIVIDE DIVIDEEQ MOD
%token <int> SPACE
%token DEF IF ELSE FOR WHILE BREAK CONTINUE RETURN IN PRINT
%token AND OR NOT TRUE FALSE NONE
%token <string> IDENTIFIER STRING 
%token <int> INT

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
  | DEF; i=id; LPAREN; fl=id_lst; RPAREN; COLON; sl=suite { Function (i , fl, sl) }
  | IF; e=exp; COLON; s1=suite; ELSE; COLON; s2=suite { IfElse (e, s1, s2) }
  | IF; e=exp; COLON; s=suite; { IfElse (e, s, []) }
  | RETURN; e=exp { Return e }
  | WHILE; e=exp COLON; s=suite; { While (e, s) }
  | CONTINUE { Continue }
  | BREAK { Break }
  | PRINT; LPAREN; e=exp RPAREN { Print e }
  | al=assign_lst; EQ; e=exp; { Assign (al, [e]) }
  | i=IDENTIFIER; PLUSEQ; e2=exp { Assign ([Identifier i], [BinaryOp (Identifier i, Plus, e2)]) }
  | i=IDENTIFIER; MINUSEQ; e2=exp { Assign ([Identifier i], [BinaryOp (Identifier i, Minus, e2)]) }
  | i=IDENTIFIER; TIMESEQ; e2=exp { Assign ([Identifier i], [BinaryOp (Identifier i, Times, e2)]) }
  | i=IDENTIFIER; DIVIDEEQ; e2=exp { Assign ([Identifier i], [BinaryOp (Identifier i, Divide, e2)]) }
  ;

assign_lst:
  | i=id { [i] }
  | al=assign_lst; EQ; i=id { al@[i] }
  ;


exp:
  | e1=exp; PLUS; e2=exp { BinaryOp (e1, Plus, e2) }
  | e1=exp; MINUS; e2=exp { BinaryOp (e1, Minus, e2) }
  | e1=exp; TIMES; e2=exp { BinaryOp (e1, Times, e2) }
  | e1=exp; DIVIDE; e2=exp { BinaryOp (e1, Divide, e2) }
  | e1=exp; MOD; e2=exp { BinaryOp (e1, Mod, e2) }
  | e1=exp; EQEQ; e2=exp { BinaryOp (e1, EqEq, e2) }
  | e1=exp; NEQ; e2=exp { BinaryOp (e1, NEq, e2) }
  | e1=exp; LT; e2=exp { BinaryOp (e1, Lt, e2) }
  | e1=exp; LEQ; e2=exp { BinaryOp (e1, LEq, e2) }
  | e1=exp; GT; e2=exp { BinaryOp (e1, Gt, e2) }
  | e1=exp; GEQ; e2=exp { BinaryOp (e1, GEq, e2) }
  | e1=exp; AND; e2=exp { BinaryOp (e1, And, e2) }
  | e1=exp; OR; e2=exp { BinaryOp (e1, Or, e2) }
  | LPAREN; e=exp; RPAREN; { e }
  | MINUS; e=exp %prec UMINUS { UnaryOp (UMinus, e) }
  | NOT; e=exp %prec NOT { UnaryOp (Not, e) }
  | TRUE { Literal (BooleanLiteral true) }
  | FALSE { Literal (BooleanLiteral false) }
  | i=INT { Literal (IntegerLiteral (i)) }
  | s=STRING { Literal (StringLiteral (s)) }
  | NONE { Literal (NoneLiteral) }
  | i=IDENTIFIER { Identifier i }
  | e=exp; LPAREN; el=exp_lst; RPAREN { Call (e, el) }
  ;

suite:
  | NEWLINE; INDENT; sl=stmts; DEDENT { sl }
  ;

id_lst:
  | { [] }
  | ir=id_rest; { ir }
  ;

id_rest:
  | i=id { [i] }
  | ir=id_rest; COMMA; i=id { ir@[i] }
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
