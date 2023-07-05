%token STATE
%token REQ
%token DEFEQ
%token INIT
%token TRANSITION
%token WHEN
%token EQ
%token NEQ
%token NOT
%token AND
%token OR
%token LT
%token LE
%token GT
%token GE
%token PLUS
%token MINUS
%token <bool> BOOL
%token <int> INT
%token <string> ID
%token BOOL_SORT
%token INT_SORT
%token ARRAY_SORT
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COLON
%token COMMA
%token SEMI
%token TICK
%token EOF

%start <Syntax.spec> spec
%start <Syntax.term> pred
%{ open Syntax %}

%%
state:
 | STATE; ds = list(state_decl);
   { mk_state ds }

state_decl:
 | b = binder; SEMI { b }

binder:
 | x = ID; COLON; s = sort { mk_decl x s }

sort:
 | BOOL_SORT { Bool }
 | INT_SORT { Int }
 | ARRAY_SORT; t1 = sort; t2 = sort { Array (t1, t2) }

term:
 | t = term; AND; u = term { Bop(And, t, u) }
 | t = term; OR; u = term { Bop(Or, t, u) }
 | e = equality  { e }

equality:
 | t = compare; EQ; u = compare { Bop(Eq, t, u) }
 | t = compare; NEQ; u = compare { Bop(Neq, t, u) }
 | c = compare { c }

compare:
 | t = arith; LT; u = arith { Bop(Lt, t, u ) }
 | t = arith; GT; u = arith { Bop(Gt, t, u ) }
 | t = arith; LE; u = arith { Bop(Le, t, u ) }
 | t = arith; GE; u = arith { Bop(Ge, t, u ) }
 | t = arith { t }

arith:
 | t = unary; PLUS; u = unary { Bop(Plus, t, u) }
 | t = unary; MINUS; u = unary { Bop(Minus, t, u) }
 | u = unary { u }

unary:
 | NOT; t = primary { Not(t) }
 | p = primary { p }

primary:
 | a = primary; LEFT_BRACKET; p = term; RIGHT_BRACKET { Get (a, p) }
 | a = primary; LEFT_BRACKET; p = term; DEFEQ; v = term; RIGHT_BRACKET { Set(a, p, v) }
 | b = BOOL { Bool b }
 | i = INT { Int i }
 | x = ID; TICK { Next x }
 | x = ID { Var x }
 | LEFT_PAREN; t = term; RIGHT_PAREN { t }

init:
  INIT; DEFEQ; t = term; { t }

pred:
  b = term; EOF { b }

transition_decl:
   | name = ID; LEFT_PAREN; bs = separated_list(COMMA, binder); RIGHT_PAREN { (name, bs) }

transition:
  | TRANSITION; d = transition_decl; WHEN; g = term; DEFEQ; t = term;
    { mk_trans (fst d) (snd d) g t }
  | TRANSITION; d = transition_decl; DEFEQ; t = term;
    { mk_trans (fst d) (snd d) (Bool true) t }

transitions:
  ts = list(transition) { ts }

requirement:
  | REQ; id = ID; DEFEQ; r = term { (id, r) }

requirements:
  ps = list(requirement) { ps }

spec:
  s = state; i = init; ts = transitions; rs = requirements; EOF { mk_spec s i ts rs }
