let compile s =
  let cmd_list = Parser.cmds Lexer.read (Lexing.from_string s) in
  Array.of_list cmd_list
