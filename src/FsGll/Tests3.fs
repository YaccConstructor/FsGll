module FsGll.Tests3

open FsGll.PureParsers
open FsGll.Parsers
open FSharpx.Prelude
open System.Diagnostics
open FsGll.ExtCalcLexer
open System.Collections.Generic

let tokenizer s = 
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString s
    Seq.initInfinite (fun _ -> FsGll.ExtCalcLexer.tokenize lexbuf) 
    |> Seq.cache
    |> Seq.takeWhile (function | EOF -> false | _ -> true)
    |> Seq.toList


type E = 
    | EM of E * E
    | EP of E * E
    | EVar of string
    | EVal of int

let runExtCalc (inp) = 
    let tok t = satisfy ((=)t)
    let term, termRef = createParserForwardedToRef<Lexem, E>("term")
    let expr, exprRef = createParserForwardedToRef<Lexem, E>("expr")
    //let vars = new Dictionary<string, E>()
    let evalVar v = EVal 5
        
    let additiveOp = tok PLUS
    let multOp     = tok MULT
    let var        = satisfy (function IDENTIFIER _ -> true | _ -> false) |>> (function IDENTIFIER x -> EVar x | _ -> failwith "er")
    let factor     = (tok LPAREN >>. expr .>> tok RPAREN) <|> (var |>> evalVar)
    termRef       := pipe3 factor multOp term (fun a x b -> match x with MULT -> EM (a, b) | _ -> failwith "er")
    exprRef       := pipe3 term additiveOp expr (fun a x b -> match x with PLUS -> EP (a, b) | _ -> failwith "er")
    
    let assign = (var .>> tok EQ) .>>. expr
    //stmts := pipe2 stmts assign (fun (vals, s) -> )
    let pgm = many1 assign .>>. expr
    tokenizer inp |> runParser pgm

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