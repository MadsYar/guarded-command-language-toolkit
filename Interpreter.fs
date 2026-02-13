module Interpreter

open Types
open Graph
open System
open AST
open Parse

(*
    This defines the input and output for the interpreter. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)
type InterpreterMemory =
    { variables: Map<string, int>
      arrays: Map<string, List<int>> }

type Input =
    { determinism: Determinism
      assignment: InterpreterMemory
      trace_length: int }

type Node = Graph.Node

type TerminationState =
    | Running
    | Stuck
    | Terminated

type Configuration<'node> = 
    { node: 'node
      memory: InterpreterMemory }

type Output =
    { execution_sequence: List<Configuration<string>>
      final: TerminationState }

let stringifyNode (internalNode: Node) : string =
    internalNode.ToString()

let prepareConfiguration (c: Configuration<Node>) : Configuration<string> =
    { node = stringifyNode c.node
      memory = c.memory }



// HELPER FUNCTIONS

let liftOpt (f, o1, o2) =
    match (o1, o2) with 
    | (Some(v1), Some(v2)) -> Some(f v1 v2)
    | _ -> None

let AddtoMap (map: Map<string, List<int>>, name: string, i: int, j: int) : Map<string, List<int>> = 
    match Map.tryFind name map with
    | Some list -> map.Add(name, List.updateAt i j list)
    | None -> failwith "Hey"

// SEMANTICS

let rec A (e: expr, sigma: InterpreterMemory) : Option<int> =
    match e with
    | Variable(x) -> sigma.variables.TryFind(x)
    | Num(x) -> Some(x)
    | PlusExpr(x1, x2) -> liftOpt (( + ), A(x1, sigma), A(x2, sigma))
    | MinusExpr(x1, x2) -> liftOpt (( - ), A(x1, sigma), A(x2, sigma))
    | TimesExpr(x1, x2) -> liftOpt (( * ), A(x1, sigma), A(x2, sigma))
    | PowExpr(x1, x2) -> match A(x1, sigma), A(x2, sigma) with
                         | Some x1, Some x2 when x2 >= 0 -> Some(int(float(x1)**float(x2)))
                         | _ -> None
    | DivExpr(x1, x2) -> match A(x1, sigma), A(x2, sigma) with
                         | Some x1, Some x2 when x2 > 0 -> Some(x1/x2)
                         | _ -> None
    | UMinusExpr(x) -> match A(x, sigma) with 
                        | Some x -> Some(-x)
                        | _ -> None
    | Array(x1, x2) -> if sigma.variables.TryFind(x1).IsSome && A(x2, sigma).IsSome 
                       then match Map.find x1 sigma.arrays with
                            | list -> match A(x2, sigma) with
                                        | Some x -> Some(List.item x list)
                                        | _ -> None
                       else None
  

let rec B (b: Bool, sigma: InterpreterMemory) : Option<bool> = 
    match b with 
    | Bool b -> Some(b)
    | And(b1, b2) -> liftOpt(( && ), B(b1, sigma), B(b2, sigma)) 
    | SAnd(b1, b2) -> liftOpt(( && ), B(b1, sigma), B(b2, sigma)) 
    | Or(b1, b2) -> liftOpt(( || ), B(b1, sigma), B(b2, sigma))  
    | SOr(b1, b2) -> liftOpt(( || ), B(b1, sigma), B(b2, sigma))
    | Not b -> match B(b, sigma) with
                     | Some b -> Some(not b)
                     | _ -> None
    | Eqv(b1, b2) -> liftOpt(( = ), A(b1, sigma), A(b2, sigma))
    | Diff(b1, b2) -> liftOpt(( <> ), A(b1, sigma), A(b2, sigma))
    | Greater(b1, b2) -> liftOpt(( > ), A(b1, sigma), A(b2, sigma))
    | GreaterEqv(b1, b2) -> liftOpt(( >= ), A(b1, sigma), A(b2, sigma))
    | Less(b1, b2) -> liftOpt(( < ), A(b1, sigma), A(b2, sigma))
    | LessEqv(b1, b2) -> liftOpt(( <= ), A(b1, sigma), A(b2, sigma))
   

let rec semantics (lab: Label, sigma: InterpreterMemory) : Option<InterpreterMemory> =
    Console.Error.WriteLine(lab.ToString())
    match lab with
    | Com c -> semanticsC(c, sigma)
    | BolDet (b, _) ->  let bval = B(b, sigma)
                        if bval.IsSome && bval.Value then Some(sigma) else None  
    | Bol b -> let bval = B(b, sigma)
               if bval.IsSome && bval.Value then Some(sigma) else None 
    | _ -> None


and semanticsC (C: Command, sigma: InterpreterMemory) : Option<InterpreterMemory> =
    match C with
    | Skip -> Some(sigma)
    | Assign(a1, a2) -> if A(a2, sigma).IsSome && sigma.variables.TryFind(a1).IsSome 
                        then let value = match A(a2, sigma) with
                                         | Some i -> i
                                         | _ -> failwith "invalid number"
                             Some( { variables= sigma.variables.Add(a1, value); arrays= sigma.arrays }) else None
    | AssignArray(a1, a2, a3) -> if A(a2, sigma).IsSome && A(a3, sigma).IsSome && sigma.arrays.TryFind(a1).IsSome && A(a2, sigma).Value < sigma.arrays.TryFind(a1).Value.Length && A(a2, sigma).Value >= 0 
                                 then let val1, val2 = match A(a2, sigma),  A(a3, sigma) with
                                                       | Some i, Some j -> i, j
                                                       | _ -> failwith "invalid number"
                                      Some( { variables= sigma.variables; arrays= AddtoMap(sigma.arrays, a1, val1, val2) }) else None                
    | _ -> None




// RUN PART

let rec execution_step (pg: List<Edge>, q: String, sigma: InterpreterMemory) =
    match pg with
    | [] -> []
    | e::rest -> 
            let lab = e.label
            let t = e.target
            let s = e.source
            if not (s.Equals(q)) then execution_step (rest, q, sigma)
            else
                let sigma_prime = semantics (lab, sigma)
                match sigma_prime with
                | Some sigma_prime -> [{node = t; memory = sigma_prime}] @ execution_step (rest, q, sigma)
                | None -> execution_step (rest, q, sigma)

let analysis (src: string) (input: Input) : Output =
    let rec execution_sequenceFUN(pg, q:Node, sigma, tl): List<Configuration<Node>> =
        let next_states = execution_step (pg, q, sigma)
        match tl, next_states with
        | 0, _ -> [{node = q; memory = sigma}]
        | _, [] -> [{node = q; memory = sigma}]
        | _, conf::_ -> [{node = q; memory = sigma}] @ execution_sequenceFUN (pg, conf.node, conf.memory, tl-1) 
                          // let execution_sequence' = execution_sequenceFUN(pg, conf.node, conf.memory, tl)
                          // { execution_sequence= {node = q; memory = sigma} execution_sequence'; final= Running}
                          // What to do with "rest" above?

    let rec final ls: TerminationState = 
        match ls with
        | [] -> Stuck
        | conf::rest -> 
            match conf.node with
            | "qF" -> Terminated
            | _ -> final rest 

    match input.determinism with
    | Deterministic ->   
        match parse Parser.startGCL (src) with
        | Ok ast ->
            let programGraph = ast2pgD(ast)
            let trace = execution_sequenceFUN (programGraph, "qS", input.assignment, input.trace_length)
            { execution_sequence = trace; final = final trace }
        | Error e -> { execution_sequence = []; final = Stuck }  

    | NonDeterministic-> 
        match parse Parser.startGCL (src) with
        | Ok ast ->
            let programGraph = ast2pg(ast)
            let trace = execution_sequenceFUN (programGraph, "qS", input.assignment, input.trace_length)            
            { execution_sequence = trace; final = final trace }
        | Error e ->{ execution_sequence = []; final = Stuck }
