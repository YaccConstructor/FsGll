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

let warmup () =
    let gcTimeout = 500
    let ra, rp, rz = TestsNonPure.runNnn (10), TestsPure.runNnn (10), TestsFParsec.runExtCalc("a =   0; 1 + 2 + (a * 2) - 223")
    printfn "Warmup: %d, %d, %A" ra.Length rp.Length rz
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

let generateExtCalcTests cnt cpath = 
    for i in 0 .. cnt do
        let n = (i + 6) * 2
        let s = genExtCalc (max 2 <| n / 5) n
        let file = @"ectest." + string i + ".txt"
        printfn "Generated %d symbols (%s)" (s.Length) file
        File.WriteAllLines(cpath + @"\" + file, [| s |])

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
    Console.ReadLine () |> ignore

let measurePerformance test (n: string option) (log: string option) (ecpath: string option) =
    let withECPath f = match ecpath with 
        | Some(ecpath) -> f ecpath
        | None -> printfn "ecpath parameter required"
    let withN f = match n with
        | Some(n) ->
            match Int32.TryParse(n) with
            | true, n -> f n
            | _, _ -> printfn "N must be Int32"
        | _-> printfn "N required"
    
    let logRecord test n (time: TimeSpan) (log: string option) =
        let s = test + " " + string n + " " + string time.TotalMilliseconds
        match log with
        | Some(log) -> [| s |] |> (curry File.AppendAllLines) log
        | _ -> printfn "%s" s

//    let extCalcInput n =
//        let r = genExtCalc (max 2 <| n / 5) n
//        printfn "extCalcInput: %s" r
//        r
    let extCalcInput (n: int) ecpath =
        let r = File.ReadAllText (ecpath + @"\" + @"ectest." + string n + ".txt")
        printfn "extCalcInput: %s" r
        r

    match test with
    | "nnn" -> withN <| fun n ->
        let res, time = measureTime (fun _ -> TestsNonPure.runNnn n)
        printfn "%A" res
        logRecord test n time log
    | "nnn-pure" -> withN <| fun n ->
        let res, time = measureTime (fun _ -> TestsPure.runNnn n)
        printfn "%A" res
        logRecord test n time log
    | "extc" -> withN <| fun n ->  withECPath <| fun ecpath ->
        let inp = extCalcInput n ecpath
        let res, time = measureTime (fun _ -> TestsNonPure.runExtCalc inp)
        printfn "%A" res
        logRecord test n time log
    | "extc-pure" -> withN <| fun n -> withECPath <| fun ecpath ->
        let inp = extCalcInput n ecpath 
        let res, time = measureTime (fun _ -> TestsPure.runExtCalc inp)
        printfn "%A" res
        logRecord test n time log
    | "extc-fparsec" -> withN <| fun n ->  withECPath <| fun ecpath ->
        let inp = extCalcInput n ecpath
        let res, time = measureTime (fun _ -> TestsFParsec.runExtCalc inp)
        printfn "%A" res
        logRecord test n time log
    | "extc-fslex" -> withN <| fun n ->  withECPath <| fun ecpath ->
        let inp = extCalcInput n ecpath
        let res, time = measureTime (fun _ -> TestsNonPure.runExtCalcFslex inp)
        printfn "%A" res
        logRecord test n time log
    | "extc-fslex-pure" -> withN <| fun n -> withECPath <| fun ecpath ->
        let inp = extCalcInput n ecpath
        let res, time = measureTime (fun _ -> TestsPure.runExtCalcFslex inp)
        printfn "%A" res
        logRecord test n time log
    | "generate-ec" -> 
        withN <| fun n -> withECPath <| fun ecpath ->  generateExtCalcTests n ecpath
    | "chart-simple" ->
        chartSimple n log
    | "debug" ->
        doDebug()
    | _ -> printfn "Unknown testcase"

//    let sw = new Stopwatch()
//    sw.Start()
//    let a = badGrammar(90)
//    printfn "Done, parser time: %A" sw.Elapsed
//    printfn "%A" a
//    let rec loop l = function
//        | EOF -> l
//        | x -> loop (x :: l) (FsGll.ExtCalcLexer.tokenize lexbuf)

//let toks = loop [] (FsGll.ExtCalcLexer.tokenize lexbuf) |> List.rev |> List.toArray
