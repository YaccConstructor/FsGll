module FsGll.Tests3

open System
open System.IO
open System.Drawing
open System.Windows.Forms
open System.Diagnostics
open System.Text.RegularExpressions
open System.Reactive.Linq
open FSharpx.Prelude


open FsGll.ExtCalcLexer
open FsGll.PureParsers
//open FsGll.AbstractParsers

type E = 
    | EM of E * char * E
    | EP of E * char * E
    | EUnary of Lexem * E
    | EVar of string
    | EVal of float
    | EAssign of E * E
    | EPgm of E list * E

let tokenizer s = 
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString s
    Seq.initInfinite (fun _ -> FsGll.ExtCalcLexer.tokenize lexbuf) 
    |> Seq.cache
    |> Seq.takeWhile (function | EOF -> false | _ -> true)
    |> Seq.toList

let extCalcParser () = 
    let factor, factorRef = createParserForwardedToRef<Lexem, E>("factor")
    let term, termRef     = createParserForwardedToRef<Lexem, E>("term")
    let expr, exprRef     = createParserForwardedToRef<Lexem, E>("expr")
    
    let tok t = satisfy ((=)t)

    let assign         = tok EQ
    let semicolon      = tok SEMICOLON
    let lparen, rparen = tok LPAREN, tok RPAREN
    let value          = satisfy (function FLOAT _ | INT _ -> true | _ -> false) |>> (function FLOAT x -> EVal x | INT x -> EVal <| float x | _ -> failwith "er")
    let variable       = satisfy (function IDENTIFIER _ -> true | _ -> false) |>> (function IDENTIFIER x -> EVar x | _ -> failwith "er")    
    let minus          = tok MINUS
    let multOp         = tok MULT <|> tok DIV
    let additiveOp     = tok PLUS <|> minus
    
    factorRef     := value <|> variable <|> (pipe2 minus factor (curry EUnary)) <|> (lparen >>. expr .>> rparen)
    termRef       := pipe3 factor multOp term (fun a x b -> match x with MULT -> EM (a, '*', b) | _ -> failwith "er") <|> factor
    exprRef       := pipe3 term additiveOp expr (fun a x b -> match x with PLUS -> EP (a, '+', b) | _ -> failwith "er") <|> term
    let stmt = pipe2 (variable .>> assign) (expr .>> semicolon) (curry EAssign)
    let pgm = pipe2 (many stmt) expr (curry EPgm)
    stmt


let extCalcLexerAndParser () = 
    let factor, factorRef = createParserForwardedToRef<char, E>("factor")
    let term, termRef     = createParserForwardedToRef<char, E>("term")
    let expr, exprRef     = createParserForwardedToRef<char, E>("expr")

    let chr c = satisfy ((=)c)
    let whitespace = satisfy <| fun c -> c = ' ' || c = '\n' || c = '\r' || c = '\t'
    let bws p = many whitespace >>. p .>> many whitespace

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
    exprRef :=  (pipe2 expr expr (+)) <|> (digit)
    expr

let runNnn (n) = 
    let nnn = nnnParser ()
    let inp = String.replicate n "0"
    inp |> runParser nnn


let runExtCalcFslex (inp) = 
    let eclp = extCalcParser ()
    let tokens = tokenizer inp
    tokens |> runParser eclp

let runExtCalc (inp) = 
    let ecp = extCalcLexerAndParser ()
    inp |> runParser ecp

open FSharp.Charting

let measureTime f = 
    let sw = new Stopwatch()
    sw.Start()
    let r = f ()
    (r, sw.Elapsed)

let doChart () =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false
    let form = new Form(Visible = true, TopMost = true, 
                            Width = 700, Height = 500)
    let context = WindowsFormsSynchronizationContext.Current
    printfn "context: %A" context
    let ev1 = new Event<_>()
    let rec loop n = async {

        let res, time = measureTime (fun _ -> runNnn n)
        printfn "time(%A) res(%d): %A" (time.TotalSeconds) n res
        context.Post(Threading.SendOrPostCallback(fun _ -> ev1.Trigger (n, time.TotalSeconds) |> ignore), null)
        return! loop (n + 2)
    } 
    loop 4 |> Async.Start 
    
    let rand = new Random()
    let obs = Observable.Interval(TimeSpan.FromSeconds(1.0)).
                 ObserveOn(WindowsFormsSynchronizationContext.Current)
                |> Observable.map(fun _ -> DateTime.Now.ToShortTimeString(),rand.Next(0,21))

    let one  = LiveChart.FastLineIncremental(ev1.Publish, Name="Simple Example")

    one.ShowChart() |> ignore
    Application.Run form

let runExample() = 
    //runExtCalc "(0+1*2 + (-3)) + 6.0 / 8"
    //runExtCalc "(0) + 2 + (a - (-5))"
    doChart ()
    //let res = runNnn 4 in printfn "res(%d): %A" 4 res
    ()

let genExtCalc cnt weight = 
    let varThreshold = 700
    let rnd = new System.Random()
    let vars = [0 .. 10] |> List.map (fun c -> int 'a' + c |> char |> string) |> List.toArray
    let var () = vars.[rnd.Next(0, vars.Length - 1)]
    let num () = match rnd.Next(0, 2) with 0 -> string <| rnd.Next(0, 100000) | _ -> string <| rnd.NextDouble()
    let operand () = if rnd.Next(0, 1000) < varThreshold then var () else num ()
    let rec genSubExpr = function
        | 1 -> operand ()
        | w -> let l = rnd.Next(1, w - 1)
               let op = match rnd.Next(0, 3) with
                        | 0 -> "  + "
                        | 1 -> " -  "
                        | 2 -> " * "
                        | 3 -> " / "
                        | _ -> failwith "e"
               let s = genSubExpr l + op + genSubExpr (w - l)
               match rnd.Next(0, 2) with 0 -> "(" + s + ")" | _ -> s
    List.concat [
        vars |> Seq.map (fun v -> v + " = 0;\n") |> Seq.toList
        [0 .. cnt] |> Seq.map (fun _ -> var () + " = " + genSubExpr weight + ";\n") |> Seq.toList
        [genSubExpr (weight / 2)]
    ]
    |> String.concat ""

let warmup () = 
    let r = runNnn (10)
    printfn "Warmup: %d" r.Length
    GC.Collect()

let doDebug () = 
    let inp = " a = 1 "
    let eclp = extCalcParser ()
    let tokens = tokenizer inp
    printfn "tokens: %A" tokens
    let res = tokens |> runParser eclp
    printfn "%A" res
    Console.ReadLine () |> ignore
    None

let measurePerformance test (n: string option) (log: string option) = 
    let withN f = 
        match n with
        | Some(n) -> 
            match Int32.TryParse(n) with 
            | true, n -> f n
            | _, _ -> Some "N must be Int32"
        | _-> Some "N required"
    let withLog f = 
        match log with
        | Some(log) -> f log
        | _ -> Some "N required"
    let logRecord test n (time: TimeSpan) (log: string option) = 
        let s = test + " " + string n + " " + string time.TotalMilliseconds
        match log with 
        | Some(log) -> [| s |] |> (curry File.AppendAllLines) log
        | _ -> printfn "%s" s

    let extCalcInput n = 
        genExtCalc 30 n
    match test with 
    | "nnn" -> withN <| fun n -> 
        warmup()
        let res, time = measureTime (fun _ -> runNnn n)
        printfn "%A" res
        logRecord test n time log
        None
    | "extc" -> withN <| fun n ->
        let inp = extCalcInput n
        warmup()
        let res, time = measureTime (fun _ -> runExtCalc inp)
        printfn "%A" res
        logRecord test n time log
        None
    | "extc-fslex" -> withN <| fun n ->
        let inp = extCalcInput n
        warmup()
        let res, time = measureTime (fun _ -> runExtCalcFslex inp)
        printfn "%A" res
        logRecord test n time log
        None
    | "debug" -> 
        doDebug()
    | _ -> Some "Unknown testcase"

//    let sw = new Stopwatch()
//    sw.Start()
//    let a = badGrammar(90)
//    printfn "Done, parser time: %A" sw.Elapsed
//    printfn "%A" a
//    let rec loop l = function 
//        | EOF -> l
//        | x -> loop (x :: l) (FsGll.ExtCalcLexer.tokenize lexbuf)

//let toks = loop [] (FsGll.ExtCalcLexer.tokenize lexbuf) |> List.rev |> List.toArray