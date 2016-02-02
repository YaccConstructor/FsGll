module FsGll.Tests.Incremental

open System
open System.Text.RegularExpressions
open FSharpx.Prelude
open FsGll.TestingCommon
open FsGll.Parsers.Incremental
open FsGll.ExtCalcLexer


let private chr c = satisfy ((=)c)
let private tok t = satisfy ((=)t)
let private whitespace = satisfy <| fun c -> c = ' ' || c = '\n' || c = '\r' || c = '\t'
let private bws p = many whitespace >>. p .>> many whitespace

let extCalcLexerAndParser () =
    let factor, factorRef = createParserForwardedToRef<char, E>("factor")
    let term, termRef     = createParserForwardedToRef<char, E>("term")
    let expr, exprRef     = createParserForwardedToRef<char, E>("expr")

    let digits   = many1 (satisfy <| fun c -> Char.IsDigit(c)) |>> (fun s -> s |> Seq.map string |> String.concat "")
    let notDigit = satisfy (fun c -> Regex.IsMatch(string c, @"[a-zA-Z_]")) |>> string
    let sym = satisfy (fun c -> Regex.IsMatch(string c, @"[0-9a-zA-Z_]")) |>> string

    let assign     = bws (chr '=')
    let semicolon  = bws (chr ';')
    let lparen, rparen = bws (chr '('), bws (chr ')')
    let value      = pipe2 digits (opt (chr '.' >>. digits)) <| fun a b -> EVal <| Double.Parse(a + defaultArg b "")
    let variable   = bws <| pipe2 notDigit (many sym) (fun a b -> EVar <| a + (b |> String.concat "") )
    let minus      = bws (chr '-')
    let multOp     = bws (chr '*') <|> bws (chr '/')
    let additiveOp = bws (chr '+') <|> minus

    factorRef     := value <|> variable <|> (pipe2 minus factor (fun _ f -> EUnary(MINUS, f))) <|> (lparen >>. expr .>> rparen)
    termRef       := pipe3 factor multOp term (fun a x b -> EM (a, x, b)) <|> factor
    exprRef       := pipe3 term additiveOp expr (fun a x b -> EP (a, x, b)) <|> term
    let stmt = pipe2 (variable .>> assign) (expr .>> semicolon) (curry EAssign)
    let pgm = pipe2 (many stmt) expr (curry EPgm)
    pgm

let runExtCalc (inp) =
    let ecp = extCalcLexerAndParser ()
    inp |> runParser ecp

let incrementalTest () = 
    let res = runExtCalc("(1*0")
    //let res = TestsNonPure.runExtCalc("1*0")
    //let res = TestsPure.runExtCalc("a = 0; var = a + 1; a - var * 2 + 837 / (x-3)   ")
    printfn "%A" res
    let partials = 
        res 
        |> List.filter (function :? GPartial<char, E> -> true | _ -> false) 
        |> List.map (fun x -> x :?> GPartial<char, E>)
    let part = List.head partials
        
    let res1 = runParser part.Parser "+1)"
    printfn "%A" res1
    let res2 = runParser part.Parser "-7)"
    printfn "%A" res2
    