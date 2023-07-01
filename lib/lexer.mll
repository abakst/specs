{
open Lexing
open Parser

exception SyntaxError of string
}

let int = '-'? ['0'-'9']['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let bool = "true" | "false"
let land = "&&"
let lor = "||"
let le = "<="
let ge = ">="
let neq = "!="


rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | "#" { read_single_line_comment lexbuf }
  | "when" { WHEN }
  | "do" { DO }
  | "state" { STATE }
  | "step" { TRANSITION }
  | "init" { INIT }
  | "Bool" { BOOL_SORT }
  | "Int" { INT_SORT }
  | "Array" { ARRAY_SORT }
  | "req" { REQ }
  | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | ":=" { DEFEQ }
  | '+' { PLUS }
  | '-' { MINUS }
  | "=" { EQ }
  | neq { NEQ }
  | '!' { NOT }
  | '<'  { LT }
  | le  { LE }
  | '>'  { GT }
  | ge  { GE }
  | land { AND }
  | lor { OR }
  | '\'' { TICK }
  | ':' { COLON }
  | ',' { COMMA }
  | ';' { SEMI }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_single_line_comment = parse
  | newline { new_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }
