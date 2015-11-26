module FsGll.Tests3

open FsGll.PureParsers
open FSharpx.Prelude
open System.Diagnostics
open FsGll.ExtCalcLexer

let runExtCalc (inp) = 
    let expr = createParserForwardedToRef("expr")
    let term = pipe3 factor additiveOp factor (fun a x b -> )
    let var    = satisfy (function | INDENTIFIER _ -> true | _ -> false)
    let eq     = satisfy (function | EQ -> true | _ -> false)
    
    let assign = (var .>> eq) .>>. expr
    let pgm = many1 assign .>>. expr |> (fun (l, a) -> l @ [a])
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString inp
    let toks = 
        Seq.initInfinite (fun _ -> FsGll.ExtCalcLexer.tokenize lexbuf) 
        |> Seq.cache
        |> Seq.takeWhile (function | EOF -> false | _ -> true)
        |> Seq.toList
        |> runParser pgm

let runExample() = 
    let res = runExtCalc " 0 + 1+ 2 * 8 -1 "


//    let sw = new Stopwatch()
//    sw.Start()
//    let a = badGrammar(90)
//    printfn "Done, parser time: %A" sw.Elapsed
//    printfn "%A" a
//    let rec loop l = function 
//        | EOF -> l
//        | x -> loop (x :: l) (FsGll.ExtCalcLexer.tokenize lexbuf)

//let toks = loop [] (FsGll.ExtCalcLexer.tokenize lexbuf) |> List.rev |> List.toArray