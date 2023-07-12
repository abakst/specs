open Syntax

module Make(S: Solver.Solver) = struct
  exception EnvLookup of string

  let lookup env x =
    match List.assoc_opt x env with
    | None -> raise (EnvLookup x)
    | Some v -> v

  let rec compile_sort ctx : sort -> S.sort = function
    | Bool -> S.bool_sort ctx
    | Int -> S.int_sort ctx
    | Array (t1, t2) -> S.array_sort ctx (compile_sort ctx t1) (compile_sort ctx t2)

  let next_name s = s ^ "!"

  let rec compile_term ctx env = function
    | Bool b -> S.bool ctx b
    | Int i -> S.int ctx i
    | Var s -> S.var ctx s (lookup env s)
    | Not t -> S.not ctx (compile_term ctx env t)
    | Bop (r, t1, t2) -> compile_bin ctx env t1 t2 r
    | Next s -> S.var ctx (next_name s) (lookup env s)
    | Get (a, p) -> S.get ctx (compile_term ctx env a) (compile_term ctx env p)
    | Set (a, p, v) -> S.set ctx (compile_term ctx env a) (compile_term ctx env p) (compile_term ctx env v)

  and compile_bin ctx env t1 t2 o =
    let e1 = compile_term ctx env t1 in
    let e2 = compile_term ctx env t2 in
    match o with
    | And -> S.conj ctx [e1; e2]
    | Or -> S.disj ctx [e1; e2]
    | Eq -> S.eq ctx e1 e2
    | Neq -> S.neq ctx e1 e2
    | Plus -> S.plus ctx e1 e2
    | Minus -> S.minus ctx e1 e2
    | Lt -> S.lt ctx e1 e2
    | Le -> S.le ctx e1 e2
    | Gt -> S.gt ctx e1 e2
    | Ge -> S.ge ctx e1 e2

  let compile_requirement ctx state (n, r) =
    let args = state |> List.map begin fun (x, s) -> S.var ctx x s, s end in
    compile_term ctx state r
    |> S.define_pred ctx n args

  let compile_transition ctx state t =
    let t_args =
      t.bindings
      |> List.map (fun (v: var_decl) -> v.name, compile_sort ctx v.sort)
    in
    let pre_state_env = state in
    let post_state_env =
      pre_state_env
      |> List.map (fun (x, s) -> (next_name x, s))
    in
    let args =
      List.map
        begin fun (x, s) ->
          (S.var ctx x s, s)
        end
        (t_args @ pre_state_env @ post_state_env)
    in
    let guard = compile_term ctx (t_args @ pre_state_env) t.guard in
    let rel   = compile_term ctx (t_args @ pre_state_env) t.step in
    let body  = S.conj ctx [guard; rel] in
    let decl  = S.define_pred ctx t.name args body in
    (t_args, decl)


  let env_of_spec ctx (spec : Syntax.spec) =
    spec.state.vars
    |> List.map (fun (d : Syntax.var_decl) ->
        (d.name, compile_sort ctx d.sort))

  let solver_transitions_of_spec ctx env (spec : Syntax.spec) =
    spec.transitions
    |> List.map (compile_transition ctx env)

  let solver_reqs_of_spec ctx env (spec : Syntax.spec) =
    spec.reqs |> List.map (compile_requirement ctx env)

  let compile ctx spec =
    let state_env = env_of_spec ctx spec in
    let init = compile_requirement ctx state_env ("init", spec.init) in
    let ts = solver_transitions_of_spec ctx state_env spec in
    let rs = solver_reqs_of_spec ctx state_env spec in
    (state_env, init, ts, rs)
end
