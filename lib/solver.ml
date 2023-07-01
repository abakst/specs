type 'model result = SAT of 'model option | UNSAT | UNKNOWN

module type Solver = sig
  type t
  type ctx
  type sort
  type term
  type decl
  type model

  val add : t -> term list -> unit
  val check : t -> term list -> model result

  val string_of_term : ctx -> term -> string
  val string_of_decl : ctx -> decl -> string

  val bool_sort : ctx -> sort
  val int_sort : ctx -> sort

  val bool : ctx -> bool -> term
  val int : ctx -> int -> term
  val var : ctx -> string -> sort -> term
  val fresh_var : ctx -> string -> sort -> term

  val subst : term -> term list -> term list -> term

  val not : ctx -> term -> term
  val eq : ctx -> term -> term -> term
  val neq : ctx -> term -> term -> term
  val conj : ctx -> term list -> term
  val disj : ctx -> term list -> term

  val plus : ctx -> term -> term -> term
  val minus : ctx -> term -> term -> term
  val lt : ctx -> term -> term -> term
  val le : ctx -> term -> term -> term
  val gt : ctx -> term -> term -> term
  val ge : ctx -> term -> term -> term

  val app : ctx -> decl -> term list -> term

  val define_pred : ctx -> string -> (term * sort) list -> term -> decl
  val decl_args: ctx -> decl -> term list
end
