module Tests3

open PureParsers
open FSharpx.Prelude
open System.Diagnostics

let tok c = satisfy ((=)c)

let badGrammar (n: int) = 
    let digit = tok '0' |>> string
    let expr, exprRef = createParserForwardedToRef<_, string>("expr")
    exprRef :=  (pipe2 expr expr (fun a b -> a + b)) <|> (digit)
    
    let s = String.replicate n "0"
    printfn "Input length: %A" (String.length s)

    let s = s :> char seq
    expr.Apply (new InputStream<_>(s, 0))

let runExample () = 
    let a = badGrammar(4)
    printfn "%A" a