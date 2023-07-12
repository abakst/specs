open Batteries
open Solver

module Make(S: Solver) = struct

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

  let apply_requirements ctx state rs =
    rs
    |> List.map (fun r -> S.app ctx r state)
    |> S.conj ctx
    |> S.not ctx
    |> List.singleton

  let check_requirements ctx state rs =
    apply_requirements ctx state rs
    |> S.check ctx

  let unroll ctx k env ts rs init =
    let rec go i state =
      if i < k then
        let step_formula, state' = step ctx state env ts in
        S.add ctx [ step_formula ];
        go (i + 1) state'
      else
        S.add ctx (apply_requirements ctx state rs)
    in
    let state0 = fresh ctx env in
    S.add ctx [ S.app ctx init state0 ];
    go 0 state0



  let bmc ctx k env ts rs init =
    let check_state state =
        let negated_prop = apply_requirements ctx state rs in
        S.check ctx negated_prop
    in
    let rec go i state =
      if i < k then
        let step_formula, state' = step ctx state env ts in
        S.add ctx [ step_formula ];
        match check_state state' with
        | Solver.SAT m -> (i, m, state')
        | Solver.UNKNOWN -> (i, None, state')
        | Solver.UNSAT -> go (i + 1) state'
      else
        (i, None, state)
    in
    print_endline ("***** Checking " ^ string_of_int k ^ " steps *****");
    let state = fresh ctx env in
    S.add ctx [ S.app ctx init state ];
    begin match check_state state with
    | Solver.UNKNOWN
    | Solver.SAT _ ->
      print_endline "Properties do not hold in initial state"
    | Solver.UNSAT ->
      let (i, _, _) = go 0 state in
      if i = k then
        print_endline ("No counterexample found of length " ^ string_of_int k)
      else
        print_endline ("Found counterexample in " ^ string_of_int i ^ " steps")
    end

end
