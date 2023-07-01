open Batteries
open Solver

module Make(_: Solver) = struct
  module S = SolverZ3.S

  let fresh ctx state =
    state |> List.map (fun (x, s) -> S.fresh_var ctx x s)

  let step ctx last_state state_env ts =
    let next_state = fresh ctx state_env in
    let inst (xs, rel) =
      let xs' = fresh ctx xs in
      S.app ctx rel (xs' @ last_state @ next_state)
    in
    let transitions = List.map inst ts in
    (S.disj ctx transitions, next_state)



  let check_requirements ctx solver i state rs =
    rs
    |> List.map (fun r -> S.app ctx r state)
    |> S.conj ctx
    |> S.not ctx
    |> List.singleton
    |> S.check solver
    |> fun res -> (i, res)

  let bmc ctx solver k env ts rs init =
    let rec go i state =
      if i < k then
        let step_formula, state' = step ctx state env ts in
        S.add solver [ step_formula ];
        match check_requirements ctx solver i state' rs with
        | (_, Solver.SAT m) -> (i, m, state')
        | _, Solver.UNSAT ->
          go (i + 1) state'
        | _, Solver.UNKNOWN -> (i, None, state')
      else
        (i, None, state)
    in
    print_endline ("***** Checking " ^ string_of_int k ^ " steps *****");
    let state = fresh ctx env in
    S.add solver [ S.app ctx init state ];
    let (i, _, _) = go 0 state in
    if i = k then
      print_endline ("No counterexample found of length " ^ string_of_int k)
    else
      print_endline ("Found counterexample in " ^ string_of_int i ^ " steps")

end
