open Batteries
open Specs
open Printf
open Lexing

open BatOptParse

let prog_name = "abst"

let unroll_opt =
  StdOpt.int_option ()

let dump_smt_opt =
  StdOpt.store_true ()

let solver_name =
  StdOpt.str_option ~default:"z3" ()

let options =
  let base = OptParser.make
      ~description:"Another bad spec tool"
      ~prog:prog_name () in
  OptParser.add
    ~help:"The maximum path length"
    ~hide:false
    ~long_name:"unroll"
    ~short_name:'k' base unroll_opt;
  OptParser.add
    ~help:"Print query as SMTLIB commands"
    ~long_name:"smtlib"
    base
    dump_smt_opt;
  OptParser.add
    ~help:"Solver (z3|z3-smtlib|cvc5)"
    ~long_name:"solver"
    base
    solver_name;
  base

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_spec parser lexbuf =
  try parser Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      exit (-1)
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let spec_from_file fn =
  let f_channel = open_in fn in
  let lexbuf = Lexing.from_channel f_channel in
  parse_spec Parser.spec lexbuf

module type ConfigType = sig
  module S: Solver.Solver
  val mk_solver : unit -> S.t
end

module Z3Config: ConfigType = struct
  module S = SolverZ3.S
  let mk_solver _ = SolverZ3.mk_solver ()
end

module SMTLIBZ3: ConfigType = struct
  module S = Smtlib.SMTLibSolver
  let mk_solver _ = Smtlib.SmtlibProcess.spawn_solver "z3" ["-in"]
end

module SMTLIBCVC5: ConfigType = struct
  module S = Smtlib.SMTLibSolver
  let mk_solver _ =
    Smtlib.SmtlibProcess.spawn_solver "cvc5"
      ["--force-logic=ALL"; "--lang=smt2"; "--interactive"; "--incremental"]
end

let main (module Cfg: ConfigType)  fn k =
  let module C = Compile.Make(Cfg.S) in
  let module B = Bmc.Make(Cfg.S) in
  let spec = spec_from_file fn in
  let solver = Cfg.mk_solver () in
  (* let solver = SolverZ3.mk_solver () in *)
  let (env, init, ts, rs) = C.compile solver spec in
  match Opt.opt dump_smt_opt with
  | Some true ->
    B.unroll solver k env ts rs init;
    print_endline (Cfg.S.dump_smt solver);
  | _ ->
    B.bmc solver k env ts rs init

let () =
  let args = OptParser.parse_argv options in
  let fn = List.hd args in
  let k = match Opt.opt unroll_opt with
          | Some k -> k
          | _ -> 3
  in
  let cfg = match Opt.get solver_name with
    | "z3" -> (module Z3Config: ConfigType)
    | "z3-smtlib" -> (module SMTLIBZ3: ConfigType)
    | "cvc5" -> (module SMTLIBCVC5: ConfigType)
    | s -> print_endline ("Unknown solver configuration: " ^ s); exit(-1)
  in
  main cfg fn k
