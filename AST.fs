// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module AST

type expr =
    | Num of int
    | TimesExpr of (expr * expr)
    | DivExpr of (expr * expr)
    | PlusExpr of (expr * expr)
    | MinusExpr of (expr * expr)
    | PowExpr of (expr * expr)
    | UMinusExpr of (expr)
    | Variable of string
    | Array of (string * expr)

type Bool =
    | Bool of bool
    | And of Bool * Bool
    | Or of Bool * Bool
    | SAnd of Bool * Bool
    | SOr of Bool * Bool
    | Not of Bool
    | Eqv of expr * expr 
    | Diff of expr * expr 
    | Greater of expr * expr 
    | GreaterEqv of expr * expr 
    | Less of expr * expr 
    | LessEqv of expr * expr 

// C  ::=  x := a  |  A[a] := a  |  skip  |  C ; C  |  if GC fi  |  do GC od
type Command = 
    | Assign of (string * expr)
    | AssignArray of (string * expr * expr)
    | Skip 
    | Sequence of Command * Command
    | If of GuardedCommand
    | Do of GuardedCommand

and GuardedCommand =
    | Condition of (Bool * Command)
    | Else of (GuardedCommand * GuardedCommand)
