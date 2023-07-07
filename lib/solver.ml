type 'model result = SAT of 'model option | UNSAT | UNKNOWN

module type Solver = sig
  type t
  type sort
  type term
  type decl
  type model

  val dump_smt : t -> string

  val add : t -> term list -> unit
  val check : t -> term list -> model result

  val string_of_term : t -> term -> string
  val string_of_decl : t -> decl -> string

  val bool_sort : t -> sort
  val int_sort : t -> sort
  val array_sort : t -> sort -> sort -> sort

  val bool : t -> bool -> term
  val int : t -> int -> term
  val var : t -> string -> sort -> term
  val fresh_var : t -> string -> sort -> term

  val subst : term -> term list -> term list -> term

  val not : t -> term -> term
  val eq : t -> term -> term -> term
  val neq : t -> term -> term -> term
  val conj : t -> term list -> term
  val disj : t -> term list -> term

  val plus : t -> term -> term -> term
  val minus : t -> term -> term -> term
  val lt : t -> term -> term -> term
  val le : t -> term -> term -> term
  val gt : t -> term -> term -> term
  val ge : t -> term -> term -> term

  val app : t -> decl -> term list -> term
  val get : t -> term -> term -> term
  val set : t -> term -> term -> term -> term

  val define_pred : t -> string -> (term * sort) list -> term -> decl
  val decl_args: t -> decl -> term list
end
