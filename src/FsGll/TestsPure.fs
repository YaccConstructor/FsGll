module FsGll.TestsPure

open System
open System.Text.RegularExpressions
open FSharpx.Prelude
open FsGll.ExtCalcLexer
open FsGll.PureParsers
open FsGll.TestingCommon

let private chr c = satisfy ((=)c)
let private tok t = satisfy ((=)t)
let private whitespace = satisfy <| fun c -> c = ' ' || c = '\n' || c = '\r' || c = '\t'
let private bws p = many whitespace >>. p .>> many whitespace

let extCalcParser () =
    let factor, factorRef = createParserForwardedToRef<Lexem, E>("factor")
    let term, termRef     = createParserForwardedToRef<Lexem, E>("term")
    let expr, exprRef     = createParserForwardedToRef<Lexem, E>("expr")

    let assign         = tok EQ
    let semicolon      = tok SEMICOLON
    let lparen, rparen = tok LPAREN, tok RPAREN
    let value          = satisfy (function FLOAT _ | INT _ -> true | _ -> false) |>> (function FLOAT x -> EVal x | INT x -> EVal <| float x | _ -> failwith "er")
    let variable       = satisfy (function IDENTIFIER _ -> true | _ -> false) |>> (function IDENTIFIER x -> EVar x | _ -> failwith "er")
    let minus          = tok MINUS
    let multOp         = tok MULT <|> tok DIV
    let additiveOp     = tok PLUS <|> minus

    factorRef     := value <|> variable <|> (pipe2 minus factor (curry EUnary)) <|> (lparen >>. expr .>> rparen)
    termRef       := pipe3 factor multOp term (fun a x b -> match x with MULT -> EM (a, '*', b) | DIV -> EM (a, '/', b) | _ -> failwith "er") <|> factor
    exprRef       := pipe3 term additiveOp expr (fun a x b -> match x with PLUS -> EP (a, '+', b) | MINUS -> EP (a, '-', b) | _ -> failwith "er") <|> term
    let stmt = pipe2 (variable .>> assign) (expr .>> semicolon) (curry EAssign)
    let pgm = pipe2 (many stmt) expr (curry EPgm)
    //pipe3 (value <|> variable) multOp (value <|> variable) (fun a x b -> (a,x,b))
    pgm

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

let nnnParser () =
    let tok c = satisfy ((=)c)
    let digit = tok '0' |>> string
    let expr, exprRef = createParserForwardedToRef<_, string>("expr")
    exprRef :=  (pipe3 expr expr expr (fun a b c -> a + b + c)) 
            <|> (pipe2 expr expr (+)) 
            <|> (digit)
    expr

let ambigExpr () = 
    let expr, exprRef     = createParserForwardedToRef<char, string>("expr")
    let num            = many1 (satisfy <| fun c -> Char.IsDigit(c)) |>> (fun s -> s |> Seq.map string |> String.concat "")
    let op             = bws (chr '+') <|> bws (chr '*') <|> bws (chr '-')
    let lparen, rparen = bws (chr '('), bws (chr ')')
    exprRef :=  (pipe3 expr op expr (fun a b c -> a + string b + c)) <|> (lparen >>. expr .>> rparen) <|> num
    expr

let runNnn (n) =
    let nnn = nnnParser ()
    let inp = String.replicate n "0"
    inp |> runParser nnn

let runExtCalcFslex (inp) =
    let eclp = extCalcParser ()
    let tokens = extCalcLexer inp
    tokens |> runParser eclp

let runExtCalc (inp) =
    let ecp = extCalcLexerAndParser ()
    inp |> runParser ecp
