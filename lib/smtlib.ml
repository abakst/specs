module Ast = Smtlib_utils.V_2_6.Ast

module SmtlibProcess = struct
  type t =
    { to_solver: out_channel
    ; from_solver: in_channel
    ; log: string list ref
    }

  let spawn_solver name args =
    let (from_solver, to_solver) = Unix.open_process_args name (Array.of_list (name :: args)) in
    { to_solver = to_solver
    ; from_solver = from_solver
    ; log = ref []
    }

  let smtlib_command_str t cmd =
    output_string t.to_solver cmd;
    output_string t.to_solver "\n";
    flush t.to_solver;
    t.log := !(t.log) @ [cmd]

  let smtlib_command_stmt t stmt =
    smtlib_command_str t (Ast.pp_to_string Ast.pp_stmt stmt)
end

module SMTLibSolver: Solver.Solver
  with
    type t = SmtlibProcess.t =
struct
  open SmtlibProcess
  type t = SmtlibProcess.t
  type sort = Ast.ty
  type term = Ast.term
  type decl = Ast.typed_var Ast.fun_decl
  type model = unit

  let dump_smt t = String.concat "\n" !(t.log)

  let (<<<) t stmt = smtlib_command_stmt t stmt

  let add t ts =
    List.iter
      (fun e -> t <<< Ast.assert_ e)
      ts

  let check t ts =
    t <<< Ast.push 1;
    add t ts;
    t <<< Ast.check_sat ();
    let res = input_line t.from_solver in
    t <<< Ast.pop 1;
    match res with
    | "sat" -> Solver.SAT None
    | "unsat" -> Solver.UNSAT
    | _ -> Solver.UNKNOWN

  let string_of_term _ = Ast.pp_to_string Ast.pp_term
  let string_of_decl _ = Ast.pp_to_string (Ast.pp_fun_decl Ast.pp_typed_var)

  let bool_sort _ = Ast.Ty_bool
  let int_sort _ = Ast.Ty_app ("Int", [])
  let array_sort _ t1 t2 = Ast.Ty_app ("Array", [t1; t2])

  let bool _ b = if b then Ast.True else Ast.False
  let int _ i = Ast.Const (string_of_int i)
  let var _ v _ = Ast.Const v

  let fresh = ref 0
  let next_fresh () =
    let n = !fresh in
    fresh := n + 1; n

  let fresh_var t v s =
    let name = v ^ "!" ^ string_of_int (next_fresh()) in
    let decl = Ast.decl_fun ~tyvars:[] name [] s in
    t <<< decl;
    Ast.const name

  let subst _ _ _ = failwith "TBD: subst"

  let not _ t = Ast.Not t
  let eq _ t1 t2 = Ast.Eq (t1, t2)
  let neq t t1 t2 = Ast.Not (eq t t1 t2)
  let conj _ ts = Ast.And ts
  let disj _ ts = Ast.Or ts

  let plus _ t1 t2 = Ast.arith Ast.Add [t1; t2]
  let minus _ t1 t2 = Ast.arith Ast.Minus [t1; t2]
  let lt _ t1 t2 = Ast.arith Ast.Lt [t1; t2]
  let le _ t1 t2 = Ast.arith Ast.Leq [t1; t2]
  let gt _ t1 t2 = Ast.arith Ast.Gt [t1; t2]
  let ge _ t1 t2 = Ast.arith Ast.Geq [t1; t2]

  let app _ decl args = Ast.app decl.Ast.fun_name args
  let get _ arr ptr = Ast.app "select" [arr; ptr]
  let set _ arr ptr value = Ast.app "update" [arr; ptr; value]

  let define_pred t name args body =
    let xs = List.map begin function
        | Ast.Const x, s -> x, s
        | _ -> failwith "Non-const as arg to define_pred"
      end args in
    let decl = Ast.mk_fun_decl ~ty_vars:[] name xs Ast.Ty_bool in
    let def = { Ast.fr_decl = decl ; Ast.fr_body = body } in
    t <<< Ast.fun_def def;
    decl

  let decl_args _ d = List.map (fun (x, _) -> Ast.Const x) d.Ast.fun_args
end
