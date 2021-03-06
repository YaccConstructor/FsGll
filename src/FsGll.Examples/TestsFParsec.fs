﻿module FsGll.TestsFParsec

open System
open System.Text.RegularExpressions
open FSharpx.Prelude
open FParsec
open FsGll.ExtCalcLexer
open FsGll.TestingCommon

let private chr c = satisfy ((=)c)
let private tok t = satisfy ((=)t)
let private whitespace = satisfy <| fun c -> c = ' ' || c = '\n' || c = '\r' || c = '\t'
let private bws p = many whitespace >>. p .>> many whitespace

let (<!>) (p: Parser<_,_>) label : Parser<_,_> = p
//    fun stream ->
//        printfn "%A: Entering %s" stream.Position label
//        let reply = p stream
//        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
//        reply

let opp = new OperatorPrecedenceParser<E, string, unit>()

let extCalcLexerAndParser () =
    let factor, factorRef = createParserForwardedToRef()
    let term, termRef     = createParserForwardedToRef()
    let expr, exprRef     = createParserForwardedToRef()

    let digits   = many1 (satisfy <| fun c -> Char.IsDigit(c)) |>> (fun s -> s |> Seq.map string |> String.concat "")
    let notDigit = satisfy (fun c -> Regex.IsMatch(string c, @"[a-zA-Z_]")) |>> string
    let sym = satisfy (fun c -> Regex.IsMatch(string c, @"[0-9a-zA-Z_]")) |>> string
    let infixOp p sep f name = attempt (pipe2 p (many (pipe2 sep p (fun a b -> (a, b)))) (fun t tail -> List.fold (fun st (op, t) -> f(st, op, t)) t tail)) <!> name

    let assign         = bws (chr '=')
    let semicolon      = bws (chr ';')
    let lparen, rparen = bws (chr '('), bws (chr ')')
    let value          = bws <| pipe2 digits (opt (chr '.' >>. digits)) (fun a b -> EVal <| Double.Parse(a + defaultArg b ""))
    let variable       = bws <| pipe2 notDigit (many sym) (fun a b -> EVar <| a + (b |> String.concat "") )
    let minus          = bws (chr '-')
    let multOp         = (bws (chr '*') <|> bws (chr '/')) <!> "multOp"
    let additiveOp     = bws (chr '+') <|> minus

    factorRef     := (attempt value <|> attempt variable <|> attempt (pipe2 minus factor (fun _ f -> EUnary(MINUS, f))) <|> (lparen >>. expr .>> rparen))    <!> "factor"
    termRef       := infixOp factor multOp EM "term"
    exprRef       := infixOp term additiveOp EP "expr"
    //exprRef       := attempt (pipe2 term (many (pipe2 additiveOp term (fun a b -> (a, b)))) (fun t tail -> List.fold (fun st (op, t) -> EP(st, op, t)) t tail)) <!> "expr"
    
    let stmt = pipe2 (variable .>> assign) (expr .>> semicolon) (curry EAssign) <!> "stmt"
    let pgm = pipe2 (attempt(many (attempt stmt))) expr (curry EPgm)
    pgm

let runExtCalc (inp) =
    let ecp = extCalcLexerAndParser ()
    run ecp inp

