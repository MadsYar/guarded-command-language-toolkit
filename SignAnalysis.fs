module SignAnalysis

open Types
open Graph
open AST
open Parse

(*
     This defines the input and output of the sign analysis. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)

type Sign =
    | Negative
    | Zero
    | Positive

type SignAssignment =
    { variables: Map<string, Sign>
      arrays: Map<string, Set<Sign>> }

type Input =
    { determinism: Determinism
      assignment: SignAssignment } // This makes up the set of initial memories

type Output =
    { initial_node: string
      final_node: string
      nodes: Map<string, Set<SignAssignment>> }
// SignAssignment = abstract memory 

type op =
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | POW
    | UMINUS

type BoolOp =
    | AND
    | OR
    | SAND
    | SOR
    | NOT
and BoolComparisonOp =
    | EQV
    | DIFF
    | GRATER
    | GRATEREQV
    | LESS
    | LESSEQV

let tableSignArithmetic op s1 s2 =
    match op with
    | PLUS -> match s1, s2 with
              | Positive, Positive -> Set[Positive]
              | Negative, Negative -> Set[Negative]
              | Zero, Zero -> Set[Zero]
              | Positive, Negative -> Set[Positive; Zero; Negative]
              | Negative, Positive -> Set[Positive; Zero; Negative]
              | Positive, Zero -> Set[Positive]
              | Zero, Positive -> Set[Positive]
              | Negative, Zero -> Set[Negative]
              | Zero, Negative -> Set[Negative]
    | MINUS -> match s1, s2 with
               | Positive, Positive -> Set[Positive; Zero; Negative]
               | Negative, Negative -> Set[Positive; Zero; Negative]
               | Zero, Zero -> Set[Zero]
               | Positive, Negative -> Set[Positive]
               | Negative, Positive -> Set[Negative]
               | Positive, Zero -> Set[Positive]
               | Zero, Positive -> Set[Negative]
               | Negative, Zero -> Set[Negative]
               | Zero, Negative -> Set[Positive] 
    | TIMES -> match s1, s2 with
               | Positive, Positive -> Set[Positive]
               | Negative, Negative -> Set[Positive]
               | Zero, Zero -> Set[Zero]
               | Positive, Negative -> Set[Negative]
               | Negative, Positive -> Set[Negative]
               | Positive, Zero -> Set[Zero]
               | Zero, Positive -> Set[Zero]
               | Negative, Zero -> Set[Zero]
               | Zero, Negative -> Set[Zero] 
    | DIV -> match s1, s2 with
              | Positive, Positive -> Set[Positive; Zero]
              | Negative, Negative -> Set[Positive; Zero]
              | Zero, Zero -> Set[]
              | Positive, Negative -> Set[Negative; Zero]
              | Negative, Positive -> Set[Negative; Zero]
              | Positive, Zero -> Set[]
              | Zero, Positive -> Set[Zero]
              | Negative, Zero -> Set[]
              | Zero, Negative -> Set[Zero] 
    | POW -> match s1, s2 with
              | Positive, Positive -> Set[Positive]
              | Negative, Negative -> Set[Positive]
              | Zero, Zero -> Set[]
              | Positive, Negative -> Set[Positive]
              | Negative, Positive -> Set[Positive;Negative]
              | Positive, Zero -> Set[Positive]
              | Zero, Positive -> Set[Zero]
              | Negative, Zero -> Set[Positive]
              | Zero, Negative -> Set[]

    | UMINUS -> match s1 with
                | Positive -> Set[Negative]
                | Negative -> Set[Positive]
                | Zero -> Set[Zero]
let tableSignBooleanOp op s1 s2 = 
    match op with
    | AND -> match s1, s2 with
             | true, true -> Set[true]
             | false, false -> Set[false]
             | true, false -> Set[false]
             | false, true -> Set[false]
    | OR -> match s1, s2 with
            | true, true -> Set[true]
            | false, false -> Set[false]
            | true, false -> Set[true]
            | false, true -> Set[true]
    | SAND -> match s1, s2 with
              | true, true -> Set[true]
              | false, false -> Set[false]
              | true, false -> Set[false]
              | false, true -> Set[false]
    | SOR -> match s1, s2 with
             | true, true -> Set[true]
             | false, false -> Set[false]
             | true, false -> Set[true]
             | false, true -> Set[true]
    | NOT -> match s1 with
             | true -> Set[false]
             | false -> Set[true]
let tableSignBooleanComparisonOp op s1 s2 = 
    match op with 
    | EQV -> match s1, s2 with
             | Positive, Positive -> Set[true; false]
             | Negative, Negative -> Set[true; false]
             | Zero, Zero -> Set[true]
             | Positive, Negative -> Set[false]
             | Negative, Positive -> Set[false]
             | Positive, Zero -> Set[false]
             | Zero, Positive -> Set[false]
             | Negative, Zero -> Set[false]
             | Zero, Negative -> Set[false]
    | DIFF -> match s1, s2 with
                 | Positive, Positive -> Set[true; false]
                 | Negative, Negative -> Set[true; false]
                 | Zero, Zero -> Set[false]
                 | Positive, Negative -> Set[true]
                 | Negative, Positive -> Set[true]
                 | Positive, Zero -> Set[true]
                 | Zero, Positive -> Set[true]
                 | Negative, Zero -> Set[true]
                 | Zero, Negative -> Set[true]
    | GRATER -> match s1, s2 with
                | Positive, Positive -> Set[true; false]
                | Negative, Negative -> Set[true; false]
                | Zero, Zero -> Set[false]
                | Positive, Negative -> Set[true]
                | Negative, Positive -> Set[false]
                | Positive, Zero -> Set[true]
                | Zero, Positive -> Set[false]
                | Negative, Zero -> Set[false]
                | Zero, Negative -> Set[true]
    | GRATEREQV -> match s1, s2 with
                   | Positive, Positive -> Set[true; false]
                   | Negative, Negative -> Set[true; false]
                   | Zero, Zero -> Set[true]
                   | Positive, Negative -> Set[true]
                   | Negative, Positive -> Set[false]
                   | Positive, Zero -> Set[true]
                   | Zero, Positive -> Set[false]
                   | Negative, Zero -> Set[false]
                   | Zero, Negative -> Set[true]
    | LESS -> match s1, s2 with
              | Positive, Positive -> Set[true; false]
              | Negative, Negative -> Set[true; false]
              | Zero, Zero -> Set[false]
              | Positive, Negative -> Set[false]
              | Negative, Positive -> Set[true]
              | Positive, Zero -> Set[false]
              | Zero, Positive -> Set[true]
              | Negative, Zero -> Set[true]
              | Zero, Negative -> Set[false]
    | LESSEQV -> match s1, s2 with
                 | Positive, Positive -> Set[true; false]
                 | Negative, Negative -> Set[true; false]
                 | Zero, Zero -> Set[true]
                 | Positive, Negative -> Set[false]
                 | Negative, Positive -> Set[true]
                 | Positive, Zero -> Set[false]
                 | Zero, Positive -> Set[true]
                 | Negative, Zero -> Set[true]
                 | Zero, Negative -> Set[false]

let rec extactSignValue op a1 a2 =
    Set.fold(fun acc sign2 -> Set.fold (fun acc2 sign1 -> Set.union (tableSignArithmetic op sign1 sign2) acc2 )acc a1) Set.empty a2
let rec extactSignValueBool op a1 a2 =
    Set.fold(fun acc sign2 -> Set.fold (fun acc2 sign1 -> Set.union (tableSignBooleanComparisonOp op sign1 sign2) acc2 )acc a1) Set.empty a2
let rec extactBool op a1 a2 =
    Set.fold(fun acc sign2 -> Set.fold (fun acc2 sign1 -> Set.union (tableSignBooleanOp op sign1 sign2) acc2 )acc a1) Set.empty a2

let rec S label (signAssignment: Set<SignAssignment>) : Set<SignAssignment> =
    match label with
    | Com (Assign(x, a)) -> Set [for SA in signAssignment do 
                                 for s in A a SA do 
                                 { variables = Map.add x s SA.variables; arrays = SA.arrays }]
    | Com (Skip) -> signAssignment
    | Bol b -> Set.fold (fun acc SA -> if Set.exists (fun sign -> sign = true) (B b SA) then Set.add SA acc else acc) Set.empty signAssignment
    //| Bol b -> Set.filter (fun SA -> Set.exists (fun sign -> sign = true) (B b SA)) signAssignment
    //| Com (AssignArray(x, a, e)) -> Set [for SA in signAssignment do 
    //                                      for s in A a SA do 
    //                                      for s2 in A e SA do
    //                                      { variables = SA.variables; arrays = Map.add x (s, s2) SA.arrays }]
    | BolDet (b, _) -> Set.fold (fun acc SA -> if Set.exists (fun sign -> sign = true) (B b SA) then Set.add SA acc else acc) Set.empty signAssignment
    | _ -> signAssignment
                                            
and A expr signAssignment : Set<Sign> = 
    match expr with
    | Num i -> if i < 0 then Set[Negative] else if i = 0 then Set[Zero] else Set[Positive]
    | Variable x -> Set[signAssignment.variables.[x]]
    | PlusExpr(e1, e2) -> extactSignValue PLUS (A e1 signAssignment) (A e2 signAssignment)
    | MinusExpr(e1, e2) -> extactSignValue MINUS (A e1 signAssignment) (A e2 signAssignment)
    | TimesExpr(e1, e2) -> extactSignValue TIMES (A e1 signAssignment) (A e2 signAssignment)
    | DivExpr(e1, e2) -> extactSignValue DIV (A e1 signAssignment) (A e2 signAssignment)
    | PowExpr(e1, e2) -> extactSignValue POW (A e1 signAssignment) (A e2 signAssignment)
    | UMinusExpr e -> extactSignValue UMINUS (A e signAssignment) Set[Zero]
    | Array(_) -> failwith "Not Implemented"

and B b signAssignment : Set<bool>= 
    match b with
    | Bool b -> if b then Set[true] else Set[false]
    | And(b1, b2) -> extactBool AND (B b1 signAssignment) (B b2 signAssignment)
    | Or(b1, b2) -> extactBool OR (B b1 signAssignment) (B b2 signAssignment)
    | SAnd(b1, b2) -> extactBool SAND (B b1 signAssignment) (B b2 signAssignment)
    | SOr(b1, b2) -> extactBool SOR (B b1 signAssignment) (B b2 signAssignment)
    | Not b -> extactBool NOT (B b signAssignment) Set[true]
    | Eqv(e1, e2) -> extactSignValueBool EQV (A e1 signAssignment) (A e2 signAssignment)
    | Diff(e1, e2) -> extactSignValueBool DIFF (A e1 signAssignment) (A e2 signAssignment)
    | Less(e1, e2) -> extactSignValueBool LESS (A e1 signAssignment) (A e2 signAssignment)
    | LessEqv(e1, e2) -> extactSignValueBool LESSEQV (A e1 signAssignment) (A e2 signAssignment)
    | Greater(e1, e2) -> extactSignValueBool GRATER (A e2 signAssignment) (A e1 signAssignment)
    | GreaterEqv(e1, e2) -> extactSignValueBool GRATEREQV (A e2 signAssignment) (A e1 signAssignment)
 
// let rec chaoticIteration (pg: list<Edge>) sigmaHat =
//     match pg with
//     | [] -> sigmaHat
//     | s1::S1 -> let qS = (S s1.label sigmaHat.nodes.[s1.source])
//                 let qF = sigmaHat.nodes.[s1.target]
//                 if Set.isSubset qS qF then chaoticIteration S1 sigmaHat
//                                 else chaoticIteration S1 {initial_node = sigmaHat.initial_node;
//                                                             final_node = sigmaHat.final_node;
//                                                             nodes = Map.add s1.target (Set.union qS qF) sigmaHat.nodes} 

let rec chaoticIterationh (pg: list<Edge>) sigmaHat =
    match pg with
    | [] -> sigmaHat
    | pg::rest when Set.isSubset (S pg.label sigmaHat.nodes.[pg.source]) (sigmaHat.nodes.[pg.target]) -> chaoticIterationh rest sigmaHat
    | pg::rest -> let mem = { initial_node = sigmaHat.initial_node;
                              final_node = sigmaHat.final_node;
                              nodes = Map.add pg.target (Set.union (S pg.label sigmaHat.nodes.[pg.source]) (sigmaHat.nodes.[pg.target])) sigmaHat.nodes }
                  chaoticIterationh rest mem

and chaoticIteration pg sigmaHat =
    let newSigmaHat = chaoticIterationh pg sigmaHat
    if sigmaHat = newSigmaHat then sigmaHat else chaoticIteration pg newSigmaHat


let setup_sigmahat pg SA =
    let States = Set.remove "qS" (Set [for s1 in pg do s1.target])
    let memory = { initial_node = "qS";
                    final_node = "qF";
                    nodes = Map["qS",Set[SA]] }

    { initial_node = memory.initial_node;
     final_node = memory.final_node;
     nodes = Map.add "qS" (Set.singleton SA) Map[for q in States do q, Set.empty] } 

    // let new_sigmaHat = {initial_node = memory.initial_node;
    //                     final_node = memory.final_node;
    //                     nodes = Map.add memory.initial_node memory.nodes.[memory.initial_node] memory.nodes}    
    
    // {initial_node = new_sigmaHat.initial_node;
    //  final_node = new_sigmaHat.final_node;
    //  nodes = Map.add new_sigmaHat.initial_node new_sigmaHat.nodes.[new_sigmaHat.initial_node] Map[for q in States do q,Set.empty]}

let analysis (src: string) (input: Input) : Output =
    match input.determinism with
    | NonDeterministic -> 
         match parse Parser.startGCL src with
         | Ok ast ->
            let programGraph = ast2pg ast
            chaoticIteration programGraph (setup_sigmahat programGraph input.assignment)
         | Error e ->  failwith "Parsing error"
    | Deterministic -> 
         match parse Parser.startGCL src with
         | Ok ast ->
            let programGraph = ast2pgD ast
            chaoticIteration programGraph (setup_sigmahat programGraph input.assignment)
         | Error e ->  failwith "Parsing error"
