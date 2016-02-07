{
open Parser
}

let register = 'r'['0' - '7']
let immediate = ['0'-'9']+
let white = [' ''t''\r''\n']+

rule read =
  parse
  | white { read lexbuf }
  | register {
    let reg_str = (Lexing.lexeme lexbuf) in
    print_endline reg_str;
    print_endline (String.sub reg_str 1 (String.length reg_str - 1)); 
    let reg_no = int_of_string (String.sub reg_str 1 (String.length reg_str - 1)) in
    REGISTER reg_no
  }
  | immediate { IMMEDIATE (int_of_string (Lexing.lexeme lexbuf)) }
  | "add" { ADD }
  | "jump0" { JUMPZERO }
  | "sub" { SUB }
  | "push" { PUSH }
  | "pop" { POP }
  | "read" { READ }
  | "write" { WRITE }
  | ";" { SEMICOLON }
  | eof { EOF }
