(**
Defines an abstract, programmable machine. A machine consists of 5 components:
  * a sequence of commands
  * a bank of registers (7, for this machine, although that constraint could be
    abstracted away)
  * a stack
  * an input stream
  * an output stream

The machine recognizes the following commands:
  * [add source1 source2 dest] sums [source1] and [source2] and stores it
    in the [dest] register. Source arguments can either be immediates (integer
    literals) or registers (denoted r[0-7]).
  * [sub source1 source2 dest] behaves like add but stores [source1] - [source2]
    in [dest]
  * [jump0 register loc] changes the program counter to [loc] if [register]
    holds the value 0.
  * [push source] stores the value in [source] at the top of the stack
  * [pop dest] removes the top of the stack and pushes it into dest. If the
    stack is empty, [pop dest] stores 0 in the destination register.
  * [read dest] removes the next word from the input stream and stores it in
    [dest]. When the input stream is exhausted, [read] stores 0 in [dest].
  * [write source] enqueues the value of [source] in the output stream

When compiling a program, write a semicolon at the end of each command. The last
semicolon is always required---if it's missing the compiler won't accept the
code.
*)

module Register = struct
  type t = int
  let compare = Pervasives.compare
  let is_valid r = 0 <= r && r < 8
end

module RegisterMap = Map.Make(Register)
type store = int RegisterMap.t
type stack = int list
module Loc = struct
  type t = int
  let is_valid l program_size = 0 <= l && l < program_size
end
module Immediate = struct
  type t = int
  let is_valid _ = true
end
type immediate = int
module Source = struct
  type t = Register of Register.t | Immediate of immediate
  let is_valid = function
    | Register r -> Register.is_valid r
    | Immediate i -> Immediate.is_valid i
end

module Cmd = struct
  type t =
    | Add      of Source.t * Source.t * Register.t
    | Sub      of Source.t * Source.t * Register.t
    | JumpZero of Register.t * Loc.t
    | Push     of Source.t
    | Pop      of Register.t
    | Read     of Register.t
    | Write    of Source.t


  let is_valid c program_size = match c with
    | Add (s1, s2, dest) ->
      Source.is_valid s1 && Source.is_valid s2 && Register.is_valid dest
    | Sub (s1, s2, dest) ->
      Source.is_valid s1 && Source.is_valid s2 && Register.is_valid dest
    | JumpZero (r, l)  -> Register.is_valid r && Loc.is_valid l program_size
    | Push s           -> Source.is_valid s
    | Pop r            -> Register.is_valid r
    | Read r           -> Register.is_valid r
    | Write s          -> Source.is_valid s

end

module Program = struct
  type t = Cmd.t array

  (** checks whether all the registers are in the range [0..7], and that all
  the jump locations are valid.
  *)
  let is_valid p =
    let prog_size = Array.length p in
    Array.fold_left (fun prev_ok c -> prev_ok && Cmd.is_valid c prog_size) true p

  (* let to_string p =
    Array. *)

end
