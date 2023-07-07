module S : Solver.Solver
  with type t = Z3.context * Z3.Solver.solver
   and type term = Z3.Expr.expr
   and type model = Z3.Model.model

val mk_solver: unit -> S.t
