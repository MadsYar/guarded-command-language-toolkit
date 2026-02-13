module Parse

open FSharp.Text.Lexing
open System
open AST

exception ParseError of Position * string * Exception

let parse parser src =
    let lexbuf = LexBuffer<char>.FromString src

    let parser = parser Lexer.tokenize

    try
        Ok(parser lexbuf)
    with
    | e ->
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new String(lexbuf.Lexeme)
        eprintf "Parse failed at line %d, column %d:\n" line column
        eprintf "Last token: %s" lastToken
        eprintf "\n"
        Error(ParseError(pos, lastToken, e))


let rec prettyPrintArithmetic ast =
    match ast with 
    | Num(a1) -> string(a1)
    | Variable(a1) -> a1
    | TimesExpr(a1, a2) -> "(" + prettyPrintArithmetic(a1) + " * " + prettyPrintArithmetic(a2) + ")"
    | DivExpr(a1, a2) -> "(" + prettyPrintArithmetic(a1) + " / " + prettyPrintArithmetic(a2) + ")"
    | PlusExpr(a1, a2) -> "(" + prettyPrintArithmetic(a1) + " + " + prettyPrintArithmetic(a2) + ")"
    | MinusExpr(a1, a2) -> "(" + prettyPrintArithmetic(a1) + " - " + prettyPrintArithmetic(a2) + ")"
    | PowExpr(a1, a2) -> "(" + prettyPrintArithmetic(a1) + "^" + "(" + prettyPrintArithmetic(a2) + ")" + ")"
    | UMinusExpr(a1) -> "(-" + prettyPrintArithmetic(a1) + ")"
    | Array(a1, a2) -> a1 + "[" + prettyPrintArithmetic(a2) + "]"

let rec prettyPrintBool ast = 
    match ast with
    | Bool(b1) -> match b1 with 
                    | true -> "true"
                    | false -> "false" 
    | And(b1, b2) -> "(" + prettyPrintBool(b1) + " & " + prettyPrintBool(b2) + ")"
    | Or(b1, b2) -> "(" + prettyPrintBool(b1) + " | " + prettyPrintBool(b2) + ")"
    | SAnd(b1, b2) -> "(" + prettyPrintBool(b1) + " && " + prettyPrintBool(b2) + ")"
    | SOr(b1, b2) -> "(" + prettyPrintBool(b1) + " || " + prettyPrintBool(b2) + ")"
    | Not(b1) ->  "!" + prettyPrintBool(b1) 
    | Eqv(b1, b2) -> "(" + prettyPrintArithmetic(b1) + " = " + prettyPrintArithmetic(b2) + ")"
    | Diff(b1, b2) -> "(" + prettyPrintArithmetic(b1) + " != " + prettyPrintArithmetic(b2) + ")"
    | Greater(b1, b2) -> "(" + prettyPrintArithmetic(b1) + " > " + prettyPrintArithmetic(b2) + ")"
    | GreaterEqv(b1, b2) -> "(" + prettyPrintArithmetic(b1) + " >= " + prettyPrintArithmetic(b2) + ")"
    | Less(b1, b2) -> "(" + prettyPrintArithmetic(b1) + " < " + prettyPrintArithmetic(b2) + ")"
    | LessEqv(b1, b2) -> "(" + prettyPrintArithmetic(b1) + " <= " + prettyPrintArithmetic(b2) + ")"

let rec prettyPrint ast =
    match ast with
    | Skip -> "skip"
    | Assign(c1, c2) -> match c1 with
                            | "if" | "fi" | "do" | "od" | "skip" -> failwithf "Can't use a keyword as a variable name"
                            | _ -> c1 + " := " + prettyPrintArithmetic(c2)
    | AssignArray(c1, c2, c3) -> match c1 with 
                                    | "if" | "fi" | "do" | "od" | "skip" -> failwithf "Can't use a keyword as a variable name"
                                    | _ -> c1 + "[" + prettyPrintArithmetic(c2) + "] := " + prettyPrintArithmetic(c3)  + "\n"
    | Sequence(c1, c2) -> prettyPrint(c1) + " ;\n" + prettyPrint(c2)
    | If(c1) -> "if " + prettyPrintGuardedCommand(c1) + "\nfi"
    | Do(c1) -> "do " + prettyPrintGuardedCommand(c1) + "\nod"

and prettyPrintGuardedCommand ast = 
    match ast with
    | Condition(g1, g2) -> prettyPrintBool(g1) + " ->\n\t" + prettyPrint(g2)
    | Else(g1, g2) -> prettyPrintGuardedCommand(g1) + "\n[] " + prettyPrintGuardedCommand(g2)


let analysis (src: string) : string =
    match parse Parser.startGCL (src) with
        | Ok ast ->
            Console.Error.WriteLine("> {0}", ast)
            prettyPrint ast
        | Error e -> "Parse error: {0}"