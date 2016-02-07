%token <int> REGISTER
%token <int> IMMEDIATE
%token ADD
%token SUB
%token JUMPZERO
%token PUSH
%token POP
%token READ
%token WRITE
%token SEMICOLON
%token EOF

%start <Machine.Cmd.t list> cmds
%%
cmds:
  | EOF                           { [] }
  | c = cmd; SEMICOLON; cs = cmds { c::cs }
  ;

cmd:
  | ADD;  s1 = source; s2 = source; dest = REGISTER
    { Machine.Cmd.Add (s1, s2, dest) }
  | SUB;  s1 = source; s2 = source; dest = REGISTER
    { Machine.Cmd.Sub (s1, s2, dest) }
  | JUMPZERO; r = REGISTER; loc = IMMEDIATE { Machine.Cmd.JumpZero (r, loc) }
  | PUSH; s = source; { Machine.Cmd.Push s }
  | POP;  r = REGISTER; { Machine.Cmd.Pop r }
  | READ; r = REGISTER; { Machine.Cmd.Read r }
  | WRITE; s = source; { Machine.Cmd.Write s }

source:
  | r = REGISTER { Machine.Source.Register r }
  | i = IMMEDIATE { Machine.Source.Immediate i }
