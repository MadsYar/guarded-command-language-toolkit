module Security
open Parse
open AST

(*
    This defines the input and output for the security analysis. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)

type Flow = { from: string; into: string }
let flow a b : Flow = { from = a; into = b }

type Classification =
    { variables: Map<string, string>
      arrays: Map<string, string> }

type Input =
    { lattice: Flow list
      classification: Classification }

type Output =
    { actual: Flow list
      allowed: Flow list
      violations: Flow list }


let rec fv ast =
  match ast with 
  | Skip -> Set.empty
  | Assign(a1, a2) -> Set.union (Set[a1]) (fvA a2)
  | Sequence(a1, a2) -> Set.union (fv a1) (fv a2)
  | If gc -> fvGC gc
  | Do gc -> fvGC gc
  | AssignArray _ -> failwith "Not implemented"
and fvA a = 
  match a with 
  | Num _ -> Set.empty
  | TimesExpr(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | DivExpr(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | PlusExpr(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | MinusExpr(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | PowExpr(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | UMinusExpr e -> fvA e
  | Variable v -> Set[v]
  | Array _ -> failwith "Not implemented"
and fvGC gc =
  match gc with 
  | Condition(b, c) -> Set.union (fvB b) (fv c)
  | Else(gc1, gc2) -> Set.union (fvGC gc1) (fvGC gc2)
and fvB b =
  match b with
  | Bool _ -> Set.empty
  | And(b1, b2) -> Set.union (fvB b1) (fvB b2)
  | Or(b1, b2) -> Set.union (fvB b1) (fvB b2)
  | SAnd(b1, b2) -> Set.union (fvB b1) (fvB b2)
  | SOr(b1, b2) -> Set.union (fvB b1) (fvB b2)
  | Not b -> fvB b
  | Eqv(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | Diff(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | Greater(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | GreaterEqv(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | Less(e1, e2) -> Set.union (fvA e1) (fvA e2)
  | LessEqv(e1, e2) -> Set.union (fvA e1) (fvA e2)


let flowRealtion a b = 
  Set.fold (fun acc b1 -> Set.fold (fun acc2 a1 -> [flow a1 b1]@acc2) acc a) List.empty b


let rec actualFlows ast X =
  match ast with 
  | Skip -> []
  | Assign(x, e) -> flowRealtion (Set.union (fvA e) X) (Set.singleton x) 
  | Sequence(c1, c2) -> (actualFlows c1 X)@(actualFlows c2 X)
  | If gc -> actualFlowsGC gc X
  | Do gc -> actualFlowsGC gc X
  | AssignArray _ -> failwith "Not implemented"
and actualFlowsGC gc X =
  match gc with
  | Condition(b, c) -> actualFlows c (Set.union (fvB b) X)
  | Else(gc1, gc2) -> (actualFlowsGC gc1 X)@(actualFlowsGC gc2 (Set.union X (implicitDeps gc1)))
and implicitDeps gc =
  match gc with 
  | Condition(b, _) -> fvB b
  | Else(gc1, gc2) -> Set.union (implicitDeps gc1) (implicitDeps gc2)


let reflexive flowList =
  let flowSet = Set.ofList flowList
  let s1 = Set[for flow in flowSet do yield ({from = flow.from; into = flow.from})]
  let s2 = Set[for flow in flowSet do yield ({from = flow.into; into = flow.into})]
  Set.union flowSet (Set.union s1 s2)
let transitive (flowSet: Set<Flow>) =
  Set[for flow in flowSet do for flow2 in flowSet do if flow.into = flow2.from then yield ({from = flow.from; into = flow2.into})]
  
let setup flowList = transitive (reflexive flowList)


let All_Variables map classs = 
        let newM= Map.filter (fun key value ->  value = classs)  map 
        Set.ofSeq(Map.keys newM)
let All_Sec set classs = 
  let s =Set.filter (fun flow -> flow.from = classs) set
  Set.map (fun flow -> flow.into) s

let rec allowedFlowsRec security classification ls= 
    match ls with
    | [] -> []
    | var::rest ->let varclass = Map.find var classification
                  let allsec = (All_Sec security varclass)
                  let sseettvars = Set.unionMany( Set.map (fun sec -> (All_Variables classification sec)) allsec)
                  Set.toList((Set.map (fun sec -> flow var sec) sseettvars)) @ (allowedFlowsRec security classification rest)
let allowedFlows security classification = allowedFlowsRec security classification (Seq.toList(Map.keys classification))
 
// Match on the rest and do not remove the var from the map



let analysis (src: string) (input: Input) : Output =
  match parse Parser.startGCL (src) with
  | Ok ast -> let actual = actualFlows ast Set.empty
              let sec = setup (input.lattice) 
              let allowed = allowedFlows (setup (Set.toList sec)) input.classification.variables
              let violations = Set.toList(Set.difference (Set.ofList actual) (Set.ofList allowed))
              { actual = List.distinct actual; 
              allowed = List.distinct allowed; 
              violations = List.distinct violations }
  | Error(errorValue) -> failwith "Not Implemented"