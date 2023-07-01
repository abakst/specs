module S : Solver.Solver
  with type ctx = Z3.context
   and type t = Z3.Solver.solver
   and type term = Z3.Expr.expr
   and type model = Z3.Model.model

val mk_context: unit -> S.ctx
val mk_solver: S.ctx -> S.t
