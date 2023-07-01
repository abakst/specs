%token STATE
%token REQ
%token DEFEQ
%token INIT
%token TRANSITION
%token WHEN
%token DO
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
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
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

bool_term:
 | t = bool_term; AND; u = bool_term { Bop(And, t, u) }
 | t = bool_term; OR; u = bool_term { Bop(Or, t, u) }
 | e = equality  { e }

equality:
 | t = compare; EQ; u = compare { Bop(Eq, t, u) }
 | t = compare; NEQ; u = compare { Bop(Neq, t, u) }
 | c = compare { c }

compare:
 | t = term; LT; u = term { Bop(Lt, t, u ) }
 | t = term; GT; u = term { Bop(Gt, t, u ) }
 | t = term; LE; u = term { Bop(Le, t, u ) }
 | t = term; GE; u = term { Bop(Ge, t, u ) }
 | t = term { t }

term:
 | t = unary; PLUS; u = unary { Bop(Plus, t, u) }
 | t = unary; MINUS; u = unary { Bop(Minus, t, u) }
 | u = unary { u }

unary:
 | NOT; t = primary { Not(t) }
 | p = primary { p }

primary:
 | b = BOOL { Bool b }
 | i = INT { Int i }
 | x = ID; TICK { Next x }
 | x = ID { Var x }
 | LEFT_PAREN; t = bool_term; RIGHT_PAREN { t }

/* term: */
/*  | b = BOOL { Bool b } */
/*  | i = INT { Int i } */
/*  | x = ID; TICK { Next x } */
/*  | x = ID { Var x } */
/*  | NOT; t = term { Not(t) } */
/*  | t1 = term; b = bin_op; t2 = term { Bop (b, t1, t2) } */
/*  | LEFT_PAREN; t = term; RIGHT_PAREN { t } */

init:
  INIT; DEFEQ; t = bool_term; { t }

pred:
  b = bool_term; EOF { b }

transition_decl:
   | name = ID; LEFT_PAREN; bs = separated_list(COMMA, binder); RIGHT_PAREN { (name, bs) }

transition:
  | TRANSITION; d = transition_decl; WHEN; g = bool_term; DEFEQ; t = bool_term;
    { mk_trans (fst d) (snd d) g t }
  | TRANSITION; d = transition_decl; DEFEQ; t = bool_term;
    { mk_trans (fst d) (snd d) (Bool true) t }

transitions:
  ts = list(transition) { ts }

requirement:
  | REQ; id = ID; DEFEQ; r = bool_term { (id, r) }

requirements:
  ps = list(requirement) { ps }

spec:
  s = state; i = init; ts = transitions; rs = requirements; EOF { mk_spec s i ts rs }
