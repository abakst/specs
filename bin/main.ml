open Batteries
open Specs
open Printf
open Lexing

module Z3Compile = Compile.Make (SolverZ3.S)
module Z3BMC = Bmc.Make(SolverZ3.S)

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
  let ctx = SolverZ3.mk_context () in
  let solver = SolverZ3.mk_solver ctx in
  let (env, init, ts, rs) = Z3Compile.compile ctx spec in
  Z3BMC.bmc ctx solver k env ts rs init

let () =
  let fn = Sys.argv.(1) in
  let k = int_of_string Sys.argv.(2) in
  main fn k
