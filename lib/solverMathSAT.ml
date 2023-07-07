open Foreign
open Ctypes

type msat_config
let msat_config:  msat_config structure typ = structure "msat_config";;
let msat_config_repr = field msat_config "repr" (ptr void);;
seal msat_config;;

type msat_env
let msat_env:  msat_env structure typ = structure "msat_env";;
let msat_env_repr = field msat_env "repr" (ptr void);;
seal msat_env;;

type msat_type
let msat_type:  msat_type structure typ = structure "msat_type";;
let msat_type_repr = field msat_type "repr" (ptr void);;
seal msat_type;;

type msat_decl
let msat_decl: msat_decl structure typ = structure "msat_decl";;
let msat_decl_repr = field msat_decl "repr" (ptr void);;
seal msat_decl;;

type msat_term
let msat_term: msat_term structure typ = structure "msat_term";;
let msat_term_repr = field msat_term "repr" (ptr void);;
seal msat_term;;

let msat_create_config = foreign
    "msat_create_config"
    (void @-> returning msat_config)

let msat_destroy_config = foreign
    "msat_destroy_config"
    (msat_config @-> returning void)

let msat_set_option = foreign
    "msat_set_option"
    (msat_config @-> Ctypes.string @-> Ctypes.string @-> returning Ctypes.int)

let msat_create_env = foreign
    "msat_create_env"
    (msat_config @-> returning msat_env)

let msat_get_integer_type = foreign
    "msat_get_integer_type"
    (msat_env @-> returning msat_type)

let msat_get_bool_type = foreign
    "msat_get_bool_type"
    (msat_env @-> returning msat_type)

let msat_get_array_type = foreign
    "msat_get_array_type"
    (msat_env @-> msat_type @-> msat_type @-> returning msat_type)

let msat_get_function_type = foreign
    "msat_get_function_type"
    (msat_env @-> ptr msat_type @-> Ctypes.int @-> msat_type @-> returning msat_type)

let msat_declare_function = foreign
  "msat_declare_function"
  (msat_env @-> Ctypes.string @-> msat_type @-> returning msat_decl)

let msat_create_itp_group = foreign
  "msat_create_itp_group"
  (msat_env @-> returning Ctypes.int)

let msat_assert_formula = foreign
  "msat_assert_formula"
  (msat_env @-> msat_term @-> returning Ctypes.int)

let msat_solve = foreign
  "msat_solve"
  (msat_env @-> returning Ctypes.int)

let msat_push_backtrack_point = foreign
  "msat_push_backtrack_point"
  (msat_env @-> returning Ctypes.int)

let msat_pop_backtrack_point = foreign
  "msat_pop_backtrack_point"
  (msat_env @-> returning Ctypes.int)

let msat_make_true = foreign
  "msat_make_true"
  (msat_env @-> returning msat_term)

let msat_make_false = foreign
  "msat_make_false"
  (msat_env @-> returning msat_term)

let msat_make_int_number = foreign
  "msat_make_int_number"
  (msat_env @-> Ctypes.int @-> returning msat_term)

let msat_make_not = foreign
  "msat_make_not"
  (msat_env @-> msat_term @-> returning msat_term)

let msat_make_or = foreign
  "msat_make_or"
  (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_iff = foreign
  "msat_make_iff"
  (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_and = foreign
  "msat_make_and"
  (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_equal = foreign
  "msat_make_equal"
  (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_constant = foreign
  "msat_make_constant"
  (msat_env @-> msat_decl @-> returning msat_term)

let msat_make_app = foreign
  "msat_make_term"
  (msat_env @-> msat_decl @-> Ctypes.ptr msat_term @-> returning msat_term)

let msat_make_array_read = foreign
  "msat_make_array_read"
  (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_array_write = foreign
  "msat_make_array_write"
  (msat_env @-> msat_term @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_plus = foreign
    "msat_make_plus"
    (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_times = foreign
    "msat_make_times"
    (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_make_leq = foreign
    "msat_make_leq"
    (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

let msat_to_smtlib2_term = foreign
  "msat_to_smtlib2_term"
  (msat_env @-> msat_term @-> returning Ctypes.string)

let msat_make_forall = foreign
  "msat_make_forall"
  (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

type placeholder = NoModel

module S: Solver.Solver
  with
    type t = msat_env structure
   and type sort = msat_type structure
   and type term = msat_term structure
   and type decl = msat_decl structure
   and type model = placeholder =
struct
  type t = msat_env structure
  type sort = msat_type structure
  type term = msat_term structure
  type decl = msat_decl structure
  type model = placeholder

  let dump_smt _ = failwith "dump_smt"

  let add t es =
    List.iter (fun e -> ignore (msat_assert_formula t e)) es

  let check t es =
    let r = msat_push_backtrack_point t in
    assert (r == 0);
    add t es;
    let result = msat_solve t in
    let r' = msat_pop_backtrack_point t in
    assert (r' == 0);
    match result with
    | -1 -> Solver.UNKNOWN
    | 0 -> Solver.UNSAT
    | _ -> Solver.SAT None

  let string_of_term e term =
    let cstr = msat_to_smtlib2_term e term in
    cstr

  let string_of_decl _ _ = failwith "string_of_decl"

  let bool_sort = msat_get_bool_type
  let int_sort = msat_get_integer_type
  let array_sort = msat_get_array_type

  let bool e b =
    if b then msat_make_true e else msat_make_false e

  let int = msat_make_int_number

  let var e nm ty =
    let decl = msat_declare_function e nm ty in
    msat_make_constant e decl

  let fresh = ref 0

  let fresh_var e nm ty =
    let nm' = nm ^ "!" ^ string_of_int !fresh in
    fresh := !fresh + 1;
    var e nm' ty

  let subst _ _ _ = failwith "subst"

  let not = msat_make_not
  let eq = msat_make_equal
  let neq e t1 t2 = msat_make_not e (msat_make_equal e t1 t2)
  let conj e = function
    | [] -> msat_make_true e
    | [t] -> t
    | t::ts -> List.fold_left (msat_make_and e) t ts

  let disj e = function
    | [] -> msat_make_false e
    | [t] -> t
    | t::ts -> List.fold_left (msat_make_or e) t ts

  let plus = msat_make_plus
  let minus e t1 t2 = plus e t1 (msat_make_times e (msat_make_int_number e (-1)) t2)
  let lt e t1 t2 = conj e [ msat_make_leq e t1 t2 ;
                            neq e t1 t2
                          ]
  let le = msat_make_leq
  let gt e t1 t2 = lt e t2 t1
  let ge e t1 t2 = le e t2 t1

  let app e f (ts : term list) =
    let ptr = Ctypes.allocate_n msat_term ~count:(List.length ts) in
    let assign i (t: term) =
      let elem_ptr = ptr +@ (i*sizeof msat_term) in
      let fld_ptr = elem_ptr |-> msat_term_repr in
      let src_ptr = t @. msat_term_repr in
      fld_ptr <-@ !@src_ptr
    in
    let _ = List.iteri assign ts in
    msat_make_app e f ptr

  let get = msat_make_array_read
  let set = msat_make_array_write

  let define_pred env f argTys body =
    let (args, tys) = List.split argTys in
    let n = List.length argTys in
    let ty_ptr = Ctypes.allocate_n msat_type ~count:n in
    let assign i (ty: sort) =
      let elem_ptr = ty_ptr +@ (i*sizeof msat_type) in
      let fld_ptr = elem_ptr |-> msat_type_repr in
      let src_ptr = ty @. msat_type_repr in
      fld_ptr <-@ !@src_ptr
    in
    let _ = List.iteri assign tys in
    let funty = msat_get_function_type env ty_ptr n (msat_get_bool_type env) in
    let decl = msat_declare_function env f funty in
    let def = msat_make_iff env (app env decl args) body in
    let rec quantify f = function
      | [] -> f
      | x::xs ->
        let t = msat_make_forall env x (quantify f xs) in
        let repr = !@ (t @. msat_term_repr) in
        assert (repr != Ctypes.null);
        t
    in
    let quantified = (quantify def args) in
    let result = msat_assert_formula env quantified in
    assert (result == 0);
    decl

  let decl_args  _ = failwith "decl_args"
end

let test =
  let cfg = msat_create_config () in
  let env = msat_create_env cfg in
  let x = S.var env "x" (S.int_sort env) in
  let t = S.eq env x (S.int env 0) in
  let u = S.int env 1 in
  let p = S.define_pred env "P" [x, S.int_sort env] t in
  let form = S.app env p [u] in
  print_endline (S.string_of_term env form);
  S.add env [form];
  S.check env []
