module SmtlibProcess : sig
  type t
  val spawn_solver : string -> string list -> t
end

module SMTLibSolver: Solver.Solver
  with
    type t = SmtlibProcess.t
