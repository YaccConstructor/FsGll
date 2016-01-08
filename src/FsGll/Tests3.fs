module FsGll.Tests3

open System
open System.IO
open System.Drawing
open System.Windows.Forms
open System.Diagnostics
open System.Reactive.Linq
open System.Text.RegularExpressions
open FSharpx.Prelude
open FSharp.Charting

open FsGll.ExtCalcLexer

module FPrsc =
    open FParsec
    open FsGll.TestingCommon
    let extCalcLexerAndParser () =
        let factor, factorRef = createParserForwardedToRef()
        let term, termRef     = createParserForwardedToRef()
        let expr, exprRef     = createParserForwardedToRef()

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

    let runExtCalc (inp) =
        let ecp = extCalcLexerAndParser ()
        run ecp inp


let warmup () =
    let gcTimeout = 500
    let ra, rp = TestsNonPure.runNnn (10), TestsPure.runNnn (10)
    printfn "Warmup: %d, %d" ra.Length rp.Length
    GC.Collect()
    System.Threading.Thread.Sleep(gcTimeout)

let measureTime f =
    warmup()
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

        let res, time = measureTime (fun _ -> TestsNonPure.runNnn n)
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
    ()

let chartSimple (n: string option) (log: string option) =
    match log with
    | Some(log) ->
        match n with
        | Some (names) ->
            let results =
                File.ReadAllLines(log)
                |> Seq.map (fun s -> s.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries))
                |> Seq.filter(fun parts -> parts.Length = 3)
                |> Seq.map (fun parts -> parts.[0], Int32.Parse(parts.[1]), Double.Parse(parts.[2]))
                |> Seq.toList
            let dataLines =
                names.Split [| ',' |]
                |> Array.map(fun name -> (name, results |> List.filter (fun (n, _, _) -> n = name) |> List.map(fun (_, n, t) -> n, t) ) )

            let chart = 
                dataLines 
                |> Array.map snd 
                |> Array.map (fun pts -> 
                    pts 
                    |> Seq.groupBy fst 
                    |> Seq.map (fun (k, p) -> let v = Seq.toList p |> List.map snd in (k, List.sum v / float (List.length v)))
                    |> Chart.Line)
                |> Chart.Combine
            let form = chart.ShowChart()
            Application.Run(form)

        | None -> failwith "error"
        //Chart.Line()
        //Application.Run
    | _ -> failwith "error"



let genExtCalc cnt weight =
    let varThreshold = 500
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
        [0 .. cnt - 1] |> Seq.map (fun _ -> var () + " = " + genSubExpr weight + ";\n") |> Seq.toList
        [genSubExpr (weight / 2)]
    ]
    |> String.concat ""

let doDebug () =
    //doChart()
    //let inp = " a = 2 + 1; "
    let inp = " a * 1 "
    
    let eclp = TestsPure.extCalcParser ()
    //let eclp = TestsNonPure.extCalcParser ()
    
    let tokens = TestingCommon.extCalcLexer inp
    printfn "tokens: %A" tokens
    
    let res = tokens |> PureParsers.runParser eclp
    //let res = tokens |> AbstractParsers.runParser eclp
    
    printfn "%A" res
    Console.ReadLine () |> ignore; None

let measurePerformance test (n: string option) (log: string option) =
    let withN f =
        match n with
        | Some(n) ->
            match Int32.TryParse(n) with
            | true, n -> f n
            | _, _ -> Some "N must be Int32"
        | _-> Some "N required"
    
    let logRecord test n (time: TimeSpan) (log: string option) =
        let s = test + " " + string n + " " + string time.TotalMilliseconds
        match log with
        | Some(log) -> [| s |] |> (curry File.AppendAllLines) log
        | _ -> printfn "%s" s

    let extCalcInput n =
        let r = genExtCalc (max 2 <| n / 5) n
        printfn "extCalcInput: %s" r
        r
    match test with
    | "nnn" -> withN <| fun n ->
        let res, time = measureTime (fun _ -> TestsNonPure.runNnn n)
        printfn "%A" res
        logRecord test n time log; None
    | "nnn-pure" -> withN <| fun n ->
        let res, time = measureTime (fun _ -> TestsPure.runNnn n)
        printfn "%A" res
        logRecord test n time log; None
    | "extc" -> withN <| fun n ->
        let inp = extCalcInput n
        let res, time = measureTime (fun _ -> TestsNonPure.runExtCalc inp)
        printfn "%A" res
        logRecord test n time log; None
    | "extc-pure" -> withN <| fun n ->
        let inp = extCalcInput n
        let res, time = measureTime (fun _ -> TestsPure.runExtCalc inp)
        printfn "%A" res
        logRecord test n time log; None
    | "extc-fparsec" -> withN <| fun n ->
        let inp = extCalcInput n
        let res, time = measureTime (fun _ -> FPrsc.runExtCalc inp)
        printfn "%A" res
        logRecord test n time log; None
    | "extc-fslex" -> withN <| fun n ->
        let inp = extCalcInput n
        let res, time = measureTime (fun _ -> TestsNonPure.runExtCalcFslex inp)
        printfn "%A" res
        logRecord test n time log; None
    | "extc-fslex-pure" -> withN <| fun n ->
        let inp = extCalcInput n
        let res, time = measureTime (fun _ -> TestsPure.runExtCalcFslex inp)
        printfn "%A" res
        logRecord test n time log; None
    | "chart-simple" ->
        chartSimple n log; None
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
