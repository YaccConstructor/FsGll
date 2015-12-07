module FsGll.Tests3

open FsGll.PureParsers
open FSharpx.Prelude
open System.Diagnostics
open FsGll.ExtCalcLexer

let runExtCalc (inp) = 
    let tok t = satisfy ((=)t)
    let expr = createParserForwardedToRef("expr")
    let var    = satisfy (function | IDENTIFIER _ -> true | _ -> false)
    
    let num    = 
        satisfy (function | INT _ -> true | _ -> false)
        |>> (function | INT x -> x | _ -> failwith "impossible") 
    
    let additiveOp = satisfy (function | PLUS | MINUS -> true | _ -> false)

    let multOp = 
        satisfy (function | PLUS | MINUS -> true | _ -> false) 
        |>> (function | PLUS -> (+) | MINUS -> (-) | _ -> failwith "impossible") 
    
    let factor = 
            tok (SYM "(") >>. expr .>> tok (SYM ")") 
        <|> num
    
    let term   = pipe3 factor multOp term (fun a x b -> (function | PLUS -> (+) | MINUS -> (-) | _ -> failwith "impossible") a b)
    expr      := pipe3 term additiveOp expr (fun a x b -> x a b)
    
    
    let assign = (var .>> tok EQ) .>>. expr
    let pgm = many1 assign .>>. expr |>> (fun (l, a) -> l @ [a])
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString inp

    let toks = 
        Seq.initInfinite (fun _ -> FsGll.ExtCalcLexer.tokenize lexbuf) 
        |> Seq.cache
        |> Seq.takeWhile (function | EOF -> false | _ -> true)
        |> Seq.toList
        |> runParser pgm
    ()

let runExample() = 
    let res = runExtCalc " 0 + 1+ 2 * 8 -1 "

    ()
//    let sw = new Stopwatch()
//    sw.Start()
//    let a = badGrammar(90)
//    printfn "Done, parser time: %A" sw.Elapsed
//    printfn "%A" a
//    let rec loop l = function 
//        | EOF -> l
//        | x -> loop (x :: l) (FsGll.ExtCalcLexer.tokenize lexbuf)

//let toks = loop [] (FsGll.ExtCalcLexer.tokenize lexbuf) |> List.rev |> List.toArray