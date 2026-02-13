module ProgramVerification

open System
open Predicate.AST
(*
    This defines the input and output for the program verification analysis.
    Please do not change the definitions below as they are needed for the
    validation and evaluation tools!
*)

type Input = unit

type Output =
    { verification_conditions: List<SerializedPredicate> }


let Update =
    let counter = ref 0
    fun () -> 
            counter.Value <- !counter + 1
            !counter


let freshVarible() = "_f" + Update().ToString()


let rec doneGC (GC: GuardedCommand) : Predicate =
    match GC with 
    | Guard(gc1, _) -> Not(gc1)
    | Choice(g1, g2) -> BooleanOp(doneGC(g1), LAnd, doneGC(g2))



let rec subP (P: Predicate, e , x) : Predicate =
    match P with
    | Bool b -> Bool b
    | RelationalOp(e1, op, e2) -> RelationalOp(subA(e1, e, x), op, subA(e2, e, x))
    | BooleanOp(p1, op, p2) -> BooleanOp(subP(p1, e, x), op, subP(p2, e, x))
    | Exists(s, p) -> Exists(s, subP(p, e, x))
    | Forall(s, p) -> Forall(s, subP(p, e, x))
    | Not p -> Not(subP(p, e, x))

and subA (e: AExpr, e', y) : AExpr  =
    match e with
    | Number i -> Number i
    | Variable s when s = y -> Variable e' 
    | Variable s -> Variable s
    | LogicalVariable s -> LogicalVariable s
    | Array(s, e) -> Array(s, subA(e, e', y))
    | LogicalArray(s, e) -> LogicalArray(s, subA(e, e', y))
    | Binary(e1, op, e2) -> Binary(subA(e1, e', y), op, subA(e2, e', y)) 
    | Function f -> subF(f, e', y)

and subF (f: Function, e', y) : AExpr = 
    match f with
    | Division(e1, e2) -> Function(Division(subA(e1, e', y), subA(e2, e', y)))
    | Min(e1, e2) -> Function(Min(subA(e1, e', y), subA(e2, e', y)))
    | Max(e1, e2) -> Function(Max(subA(e1, e', y), subA(e2, e', y)))
    | Count(s, e) -> Function(Count(s, subA(e, e', y)))
    | LogicalCount(s, e) -> Function(LogicalCount(s, subA(e, e', y)))
    | Length(s) -> Function(Length(s))
    | LogicalLength(s) -> Function(LogicalLength(s))
    | Fac E -> Function(Fac(subA(E, e', y)))
    | Fib E -> Function(Fib(subA(E, e', y)))


let analysis (src: string) (input: Input) : Output =
    let (P, C, Q) =
        match Predicate.Parse.parse src with
        | Ok (AnnotatedCommand (P, C, Q)) -> P, C, Q
        | Error e ->
            failwith
                $"Failed to parse.\n\nDid you remember to surround your program with Picate blocks, like so?\n\n  {{ true }} skip {{ true }}\n\n{e}"


    let rec spC (C: Command, P: Predicate) : Predicate =
        match C with
        | Skip -> P
        | Assign(x, a) -> let y = freshVarible()
                          Exists(y, BooleanOp(subP(P, y, x), LAnd, RelationalOp(Variable x, Eq,  subA(a, y, x))))
        //| ArrayAssign(x, a1, a2) -> 
        | Sep(c1, c2) -> spC(c2, spC(c1, P))
        | If gc -> spCgc(gc, P)
        | Do(p, gc) -> BooleanOp(p, LAnd, doneGC(gc))
        | _ -> failwith "fail"

    and spCgc (GC: GuardedCommand, P: Predicate) : Predicate =
        match GC with 
        | Guard(b, c) -> spC(c, BooleanOp(b, LAnd, P))
        | Choice(gc1, gc2) -> BooleanOp(spCgc(gc1, P), LOr, spCgc(gc2, P))

    and  vC (C: Command, R: Predicate) : List<Predicate> =
        match C with
        | Skip -> []
        | Assign _ -> []
      //| ArrayAssign(x, a1, a2) ->
        | Sep(c1, c2) -> vC(c1, R) @ vC(c2, spC(c1, R))
        | If gc -> vCgc(gc, R)
        | Do(p, gc) -> [BooleanOp(R, Implies, p); BooleanOp(spCgc(gc, p), Implies, p)] @ vCgc(gc, p)
        | _ -> failwith "fail"
    
    and vCgc (GC: GuardedCommand, R: Predicate) : List<Predicate> =
        match GC with
        | Guard(b, c) -> vC(c, BooleanOp(b, LAnd, R))
        | Choice(gc1, gc2) -> vCgc(gc1, R) @ vCgc(gc2, R)


    let verification_conditions: List<Predicate> =
        [BooleanOp(spC(C, P), Implies, Q)] @ vC(C, P)
        
    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate verification_conditions }
