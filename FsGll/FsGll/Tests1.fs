module Tests1

open Parsers
//open Parsersok
open FSharpx.Prelude
open System.Diagnostics
open Tests1

let inline  curry3 f a b c = f (a, b, c)

type Expr = 
    | Expr of Expr * Expr
    | Atom of string

let dohash x =
    x + (x <<< 3)

let revhash a = 
    seq { for i in 0L .. 7L -> a + i * (2L <<< 31) } 
    |> Seq.filter (fun v -> v % 9L = 0L) 
    |> Seq.map (flip (/) 9L)
    |> Seq.toList

let badGrammar () = 
    let digit = str "0"
    let expr, exprRef = createParserForwardedToRef<string>("expr")
    exprRef := 
            (expr -~- expr ^^ (fun a b -> a + b))
        <|> (digit)

    let br0 =  str "(" .>> (many << str) " "
    let br1 =  str ")" .>> (many << str) " "
    let brS, brSRef = createParserForwardedToRef<_>("")
    brSRef := 
            (br0 -~- brS -~- br1 ^^^ (fun a b c -> a + b + c)) 
        <|> (epsilon .^ konst "") 
        <|> (brS -~- brS ^^ (+))

    //let a = expr.Apply(new CharStream("011"))
    //let a = brS.Apply(new CharStream("(())()"))
    let s = String.replicate 100 "0"
    printfn "Input length: %A" (String.length s)
    apply expr (new CharStream(s))

type Node = 
    | Block          of Node list
    | Declaration    of string * string list * Node
    | LogicalOr      of Node * string * Node
    | LogicalAnd     of Node * string * Node
    | Additive       of Node * string * Node
    | Multiplicative of Node * string * Node
    | Unary          of string * Node
    | Application    of Node * Node
    | Primary        of string
    | IConst         of int

let pgm = String.concat "\n" [ "let inc x =        "
                               "    f x            "
                               "                   " ] 

let goodGrammar1 () =
    let declaration, declarationRef = createParserForwardedToRef<_>("declaration")
    let statement, statementRef     = createParserForwardedToRef<_>("statement")
    let logicalOr, logicalOrRef     = createParserForwardedToRef<_>("logicalOr")
    let logicalAnd, logicalAndRef   = createParserForwardedToRef<_>("logicalAnd")
    let additive, additiveRef       = createParserForwardedToRef<_>("additive")
    let term, termRef               = createParserForwardedToRef<_>("term")
    let unary, unaryRef             = createParserForwardedToRef<_>("unary")
    let postfix, postfixRef         = createParserForwardedToRef<_>("postfix")
    let appl, applRef               = createParserForwardedToRef<_>("appl")
    let primary, primaryRef         = createParserForwardedToRef<_>("primary")

    let between p1 p2 = p1 >>. p2 .>> p1    
    let identChar     =  (pdigit .^ string) <|> (pletter .^ string) <|> str "_" 
    let identifier   = ((pletter .^ string) <|> str "_") -~- (many identChar .^ List.fold (+) "") ^^ (+)
    let iconst       = many1 (pdigit .^ string) .^ List.fold (+)  ""
    let oper s      = between (opt pwhitespace) (str s)

    let parms       = many (pwhitespace >>. identifier)
    let block       = (oper "{" >>. (many (declaration <|> statement) -~- statement) .>> oper "}") ^^ (fun xs x -> xs @ [x] |> Block)
    let expr        = logicalOr
    
    statementRef   := block <|> expr
    
    declarationRef := (opt pwhitespace -~- str "let" -~- pwhitespace >>. 
                       identifier -~- parms .>> 
                       oper "=") -~- (block <|> statement) ^^^ curry3 Declaration

    logicalAndRef  := 
            logicalAnd -~- oper "&&"  -~- additive ^^^ curry3 LogicalAnd 
        <|> additive

    logicalOrRef   := 
            logicalOr -~- oper "||"  -~- logicalAnd ^^^ curry3 LogicalOr
        <|> logicalAnd
    
    primaryRef     := 
            identifier .^ Primary
        <|> (oper "(" -~- expr -~- oper ")" ^^^ (fun _ f _ -> f) )
        <|> (iconst .^ (IConst << int) )
        
    applRef        := 
            (appl .>> pwhitespace) -~- primary ^^ curry Application
        <|> primary
    
    postfixRef     := appl

    unaryRef       := 
            (oper "-" -~- unary) ^^ curry Unary 
        <|> postfix

    termRef        := 
            term -~- (oper "*" <|> oper "/") -~- unary ^^^ curry3 Multiplicative 
        <|> unary

    additiveRef    := 
            additive -~- (oper "+" <|> oper "-") -~- term ^^^ curry3 Additive
        <|> term


    //let _module = many declaration .>> opt pwhitespace
    let _module = declaration .>> opt pwhitespace
    //let _module = (opt pwhitespace -~- str "let" -~- pwhitespace >>. many (matchc (konst true)))
    apply _module (new CharStream(pgm))

let rungll1 () = 
    let L0, LS, LS0, LS0', LS1, LS2 = 0, 1, 2, 3, 4, 5
    let mutable R = new ResizeArray<int * int list * int>()
    let mutable Done = false
    let mutable ci = 0
    let inp = "aaa$"

    let push L s i = R.Add(L, s, i)
    let pop () = 
        let ind = R.Count - 1
        let res = R.[ind]
        R.RemoveAt(ind)
        res

    let mutable L = L0
    let mutable s = []
    push LS [] 0
    while not Done do
        match L with 
        | _ when L = L0 -> 
            if R.Count <> 0 then 
                let L', s', ci' = pop ()
                L  <- L'
                s  <- s'
                ci <- ci'
            else if ci = inp.Length then failwith "SUCCESS"
            else failwith "FAILURE"
        | _ when L = LS -> 
            if inp.[ci] = 'a' then
                push LS0 s ci
                push LS1 s ci
            else if inp.[ci] = '$' then
                push LS2 s ci
            L <- L0
        | _ when L = LS0 -> 
            if inp.[ci] = 'a' then 
                s <- LS0' :: s
                L <- LS
            else 
                L <- L0
        | _ when L = LS0' -> 
            let L' :: s' = s
            s <- s'
            L <- L'
        | _ when L = LS1 -> 
            if inp.[ci] = 'a' then 
                let L' :: s' = s
                s <- s'
                push L' s (ci + 1)
            L <- L0
        | _ when L = LS2 -> 
            if inp.[ci] = '$' then
                let L' :: s' = s
                s <- s'
                push L' s ci
            L <- L0

let runExample () = 
    //rungll1() 
    let sw = new Stopwatch()
    
    printfn "Started"
    sw.Start()
    
    let a = goodGrammar1()
    //let a = badGrammar()

    printfn "Finished"
    printfn "Done: %A, time: %A" (List.length a) sw.Elapsed
    a |> List.sortBy (function Failure (x, y) -> y.Ind | _ -> 0) |> List.iter (printfn "%A")

//    printfn ("All done!\n\
//  trampolinesCreated: %d\n\
//  savedToDone:   %4i  foundInDone:   %4i  notFoundInDone:   %4i\n\
//  savedToPopped: %4i  foundInPopped: %4i  notFoundInPopped: %4i\n\
//  savedToSaved:  %4i  foundInSaved:  %4i  \n\
//  queuePushed:  %4i  ")
//        trampolinesCreated
//        savedToDone foundInDone notFoundInDone
//        savedToPopped foundInPopped notFoundInPopped
//        savedToSaved foundInSaved
//        queuePushed

    // E -> E E -> E 0 
    //          -> E 1
    //          -> E E 1
    //          -> E E 1 -> 0 1 1
    //          -> E E E
    //          -> 0 1 1



