module S: Solver.Solver
  with type t = Z3.context * Z3.Solver.solver
   and type term = Z3.Expr.expr
   and type model = Z3.Model.model =
struct
  type t = Z3.context * Z3.Solver.solver
  type sort = Z3.Sort.sort
  type term = Z3.Expr.expr
  type decl = Z3.FuncDecl.func_decl
  type model = Z3.Model.model

  let string_of_term _ term =
    Z3.Expr.to_string term

  let string_of_decl _ decl =
    Z3.FuncDecl.to_string decl

  let int_sort (ctx, _) =
    Z3.Arithmetic.Integer.mk_sort ctx

  let bool_sort (ctx, _) =
    Z3.Boolean.mk_sort ctx

  let array_sort (ctx, _) s1 s2 =
    Z3.Z3Array.mk_sort ctx s1 s2

  let bool (ctx, _) b =
    if b then
      Z3.Boolean.mk_true ctx
    else
      Z3.Boolean.mk_false ctx

  let int (ctx, _) i =
    Z3.Arithmetic.Integer.mk_numeral_i ctx i

  let var (ctx, _) i s =
    Z3.Expr.mk_const_s ctx i s

  let fresh_var (ctx, _) i s =
    Z3.Expr.mk_fresh_const ctx i s

  let eq (ctx, _) t1 t2 =
    Z3.Boolean.mk_eq ctx t1 t2

  let neq (ctx, _) t1 t2 =
    Z3.Boolean.mk_distinct ctx [t1; t2]

  let not (ctx, _) t =
    Z3.Boolean.mk_not ctx t

  let conj (ctx, _) ts =
    Z3.Boolean.mk_and ctx ts

  let disj (ctx, _) ts =
    Z3.Boolean.mk_or ctx ts

  let plus (ctx, _) t1 t2 =
    Z3.Arithmetic.mk_add ctx [t1; t2]

  let minus (ctx, _) t1 t2 =
    Z3.Arithmetic.mk_sub ctx [t1; t2]

  let lt (ctx, _) t1 t2 =
    Z3.Arithmetic.mk_lt ctx t1 t2

  let le (ctx, _) t1 t2 =
    Z3.Arithmetic.mk_le ctx t1 t2

  let gt (ctx, _) t1 t2 =
    Z3.Arithmetic.mk_gt ctx t1 t2

  let ge (ctx, _) t1 t2 =
    Z3.Arithmetic.mk_ge ctx t1 t2

  let define_pred (ctx, solver) f argTys body =
    let (xs, sorts) = List.split argTys in
    let decl = Z3.FuncDecl.mk_func_decl_s ctx f sorts (bool_sort (ctx, solver)) in
    let def = Z3.Boolean.mk_eq ctx (Z3.Expr.mk_app ctx decl xs) body in
    let axiom = Z3.Quantifier.mk_forall_const ctx xs def (Some 1) [] [] None None
              |> Z3.Quantifier.expr_of_quantifier
    in
    Z3.Solver.add solver [ axiom ];
    decl

  let decl_args (ctx, _) decl =
    let ps = Z3.FuncDecl.get_parameters decl in
    let destruct p =
      Z3.Expr.mk_const ctx (Z3.FuncDecl.Parameter.get_symbol p) (Z3.FuncDecl.Parameter.get_sort p)
    in
    List.map destruct ps

  let app (ctx, _) f args =
    Z3.Expr.mk_app ctx f args

  let get (ctx, _) =
    Z3.Z3Array.mk_select ctx

  let set (ctx, _) =
    Z3.Z3Array.mk_store ctx

  let subst t froms tos =
    Z3.Expr.substitute t froms tos

  let bracket solver (f : unit -> 'a): 'a =
    Z3.Solver.push solver;
    let r = f () in
    Z3.Solver.pop solver 1;
    r

  let bracket_check solver es =
    bracket solver
      begin fun () ->
        Z3.Solver.add solver es;
        Z3.Solver.check solver []
      end

  let check (_, solver) es =
    match bracket_check solver es with
    | Z3.Solver.SATISFIABLE ->
      Solver.SAT (Z3.Solver.get_model solver)
    | Z3.Solver.UNKNOWN ->
      Solver.UNKNOWN
    | Z3.Solver.UNSATISFIABLE ->
      Solver.UNSAT

  let add (_, solver) es = Z3.Solver.add solver es

  let dump_smt (_, solver) = Z3.Solver.to_string solver

end

let mk_solver (): S.t =
  let ctx = Z3.mk_context [] in
  (ctx, Z3.Solver.mk_simple_solver ctx)
