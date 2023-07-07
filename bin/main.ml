open Batteries
open Specs
open Printf
open Lexing

open BatOptParse

module Z3Compile = Compile.Make (SolverZ3.S)
module Z3BMC = Bmc.Make(SolverZ3.S)

let prog_name = "abst"

let unroll_opt =
  StdOpt.int_option ()

let dump_smt_opt =
  StdOpt.store_true ()

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

let main fn k =
  let spec = spec_from_file fn in
  let solver = SolverZ3.mk_solver () in
  let (env, init, ts, rs) = Z3Compile.compile solver spec in
  match Opt.opt dump_smt_opt with
  | Some true ->
    Z3BMC.unroll solver k env ts rs init;
    print_endline (Z3.Solver.to_string (snd solver));
  | _ ->
    Z3BMC.bmc solver k env ts rs init

let () =
  let args = OptParser.parse_argv options in
  let fn = List.hd args in
  let k = match Opt.opt unroll_opt with
          | Some k -> k
          | _ -> 3
  in
  main fn k
