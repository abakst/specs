type sort =
  | Bool
  | Int
  | Array of (sort * sort)

type var_decl =
  { sort: sort; name: string }

type state =
  { vars: var_decl list }

type bop =
  | Eq | Neq
  | And | Or
  | Plus | Minus
  | Lt | Le | Gt | Ge

type term =
  | Bool of bool
  | Int of int
  | Var of string
  | Next of string
  | Not of term
  | Bop of (bop * term * term)

(* type ('a, 'b) array_type = *)
(*   | Array_type: ('a rep * 'b rep) -> ('a, 'b) array_type *)

(* and _ rep = *)
(*   | IntRep: int rep *)
(*   | BoolRep: bool rep *)
(*   | ArrayRep: ('a rep * 'b rep) -> ('a, 'b) array_type rep *)

(* type _ typed_bop = *)
(*   | Eq: 'a typed_bop *)
(*   | Neq: 'a typed_bop *)
(*   | And: bool typed_bop *)
(*   | Or: bool typed_bop *)
(*   | Lt: int typed_bop *)
(*   | Gt: int typed_bop *)
(*   | Le: int typed_bop *)
(*   | Ge: int typed_bop *)

(* type _ typed_var = *)
(*   | V: (string * 'a rep) -> 'a typed_var *)

(* type _ typed_term = *)
(*   | Var: 'a typed_var -> 'a typed_term *)
(*   | Bool: bool -> bool typed_term *)
(*   | Int: int -> int typed_term *)
(*   | Eq: 'a typed_term * 'a typed_term -> bool typed_term *)
(*   | Bop: 'a typed_bop * 'a typed_term * 'a typed_term -> 'a typed_term *)

type transition =
  { name: string; bindings: var_decl list; guard: term; step: term }

type spec =
  { state: state;
    init: term;
    transitions: transition list;
    reqs: (string * term) list;
  }

let mk_decl x s = { sort = s; name = x }

let mk_state ds = { vars = ds }

let mk_trans n bs g s = { name = n; bindings = bs; guard = g; step = s  }

let mk_spec s i ts rs = { state = s; init = i; transitions = ts; reqs = rs }
