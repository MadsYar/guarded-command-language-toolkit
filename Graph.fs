module Graph


open Types
open Parse
open AST

(*
    This defines the input and output for graphs. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)

type Input = { determinism: Determinism }

type Output = { dot: string }

type Node = string

type Label = 
    | Com of Command
    | Bol of Bool 
    | BolDo of string // We can do better than this
    | BolDet of Bool*string
    | BolDoDet of string

type Edge = {
        source: Node;
        label: Label;
        target: Node
}

//                                        Non-Deterministic 
// ###################################################################################################


let Update =
    let counter = ref 0
    fun () -> 
            counter.Value <- !counter + 1
            !counter


let FreshNode() = Node ("q" + Update().ToString())


// Dot notation is something like this: q▷ -> q1 [label = "skip"];      
// ▷◀  
let label2dot (l: Label) : string =
    match l with
    | Com l -> "[label = " + "\"" + prettyPrint(l) + "\"" + "]"
    | Bol l -> "[label = " + "\"" + prettyPrintBool(l) + "\"" + "]"
    | BolDo l -> "[label = " + "\"" + l + "\"" + "]"
    | BolDoDet l -> "[label = " + "\"" + "!" + l + "\"" + "]"   
    | BolDet (B, b) -> "[label = " + "\"" + prettyPrintBool(B) + " & " + "(" + "!" + b + ")" + "\"" + "]"


let addNeg (b: Bool) : string = "!" + prettyPrintBool(b)


let rec doneGC (GC: GuardedCommand) : string =
    match GC with 
    | Condition(g1, _) -> addNeg(g1)
    | Else(g1, g2) -> "(" + doneGC(g1) + " & " + doneGC(g2) + ")"


// Notation for functions below:    let func (var: type) : what it returns
let rec edges (ast: Command, qS: Node, qF: Node) : List<Edge> =
    match ast with 
    | Assign(c1, c2) -> [{source = qS;  label = Com(Assign(c1, c2)); target = qF}]
    | Skip -> [{source = qS;  label = Com(ast); target = qF}]   
    | Sequence(c1, c2) -> let q = FreshNode() 
                          edges(c1, qS, q) @ edges(c2, q, qF)
    | AssignArray(c1, c2, c3) -> [{source = qS;  label = Com(AssignArray(c1, c2, c3)); target = qF}]
    | If(GC) -> edgesGC(GC, qS, qF) 
    | Do(GC) -> let b = doneGC(GC)
                edgesGC(GC, qS, qS) @ [{source = qS;  label = BolDo(b) ; target = qF}]
    
and edgesGC (ast: GuardedCommand, qS: Node, qF: Node) : List<Edge> =
    match ast with
    | Condition(g1, g2) -> let q = FreshNode()
                           [{source = qS;  label = Bol(g1); target = q}] @ edges(g2, q, qF)
    | Else(g1, g2) -> edgesGC(g1, qS, qF) @ edgesGC(g2, qS, qF)


let ast2pg (ast: Command) : List<Edge> =
    edges(ast, "qS", "qF")


let edge2dot (e: Edge) : string =
    e.source + " -> " + e.target + " " + label2dot(e.label) + ";\n" 


let rec edges2dot (pg : List<Edge>) : string =
    match pg with
    | [] -> ""
    | e::pg -> edge2dot(e) + edges2dot(pg)


let pg2dot (pg : List<Edge>) : string =
    "digraph program_graph {rankdir=LR;
    node [shape = circle]; qS;
    node [shape = doublecircle]; qF;
    node [shape = circle]\n" + edges2dot(pg) + "}" 



//                                        Deterministic 
// ###################################################################################################


let rec edgesD (ast: Command, qS: Node, qF: Node) : List<Edge> =
    match ast with 
    | Assign(c1, c2) -> [{source = qS;  label = Com(Assign(c1, c2)); target = qF}]
    | Skip -> [{source = qS;  label = Com(ast); target = qF}]
    | Sequence(c1, c2) -> let q = FreshNode() 
                          edgesD(c1, qS, q) @ edgesD(c2, q, qF)
    | AssignArray(c1, c2, c3) -> [{source = qS;  label = Com(AssignArray(c1, c2, c3)); target = qF}]
    | If(GC) -> 
        let (e, _) = edgesGCD((GC, qS, qF), "false")
        e 
    | Do(GC) -> let b = doneGC(GC)
                let (e, d) = edgesGCD((GC, qS, qS), "false")
                e @ [{source = qS;  label = BolDoDet(d) ; target = qF}]

and edgesGCD ((ast: GuardedCommand, qS: Node, qF: Node), d: string) : List<Edge>*string =
    match ast with
    | Condition(b, g2) -> let q = FreshNode()
                          ([{source = qS;  label = BolDet(b, d); target = q}] @ edgesD(g2, q, qF), "(" + prettyPrintBool(b) + "|" + d + ")") 
    | Else(g1, g2) ->
        let (e1, d1) = edgesGCD((g1, qS, qF), d) 
        let (e2, d2) = edgesGCD((g2, qS, qF), d1)
        (e1@e2, d2)


let ast2pgD (ast: Command) : List<Edge> =
    edgesD(ast, "qS", "qF")



//                                        Run 
// ###################################################################################################

let rec analysis (src: string) (input: Input) : Output =
    match input.determinism with
        | Deterministic -> match parse Parser.startGCL (src) with
                                | Ok ast ->
                                    let pg = ast2pgD(ast)  // edges funcions
                                    // Console.Error.WriteLine("> {0}", pg)
                                    let dotstring = pg2dot(pg)
                                    { dot = dotstring }
                                | Error e -> { dot = "" }

        | NonDeterministic -> match parse Parser.startGCL (src) with
                                | Ok ast ->
                                    let pg = ast2pg(ast)  // edges funcions
                                    // Console.Error.WriteLine("> {0}", pg)
                                    let dotstring = pg2dot(pg)
                                    { dot = dotstring }
                                | Error e -> { dot = "" }
    
    

(*
    ##################### COMMANDS TO RUN TO TEST THE GRAPH PROGRAM #####################
    dotnet run graph "skip" "{ determinism :{Case: 'Deterministic'}}"
    dotnet run graph "skip" "{ determinism :{Case: 'NonDeterministic'}}"

*)