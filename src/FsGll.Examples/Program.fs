module FsGll.Examples.Program

open System
open System.Collections
open System.Collections.Generic
//open Parsers
//open Parsersok
open FSharpx.Prelude
open System.Diagnostics
open FsGll.Tests3
           
//type MyData<'a> = 
//    | Constr of 'a
//    | Nothing

//let hand (a: MyData<#Object>) =
//    let s = a :> MyData<Object>
//    ()

//let a1 = Constr("aaaa")
//let a2 = Constr(new Object())
//hand a1
                 
open FsGll.Mutable.PriorityQueue    

let printUsage() = 
    printfn "Usage: <pgm> (-run=<Run> | -test=<Test> [-n=<N>] [-log=<Log>] [-ecpath=<ExtCalc tests dir path>])"
    printfn "       Test: (nnn|nnn-pure|extc|extc-pure|extc-fparsec|extc-fslex)"
    printfn "       Run: (incr|cool-incr)"

[<EntryPoint>]
let main argv =
    //let res = TestsPure.runExtCalc("a = 0; var = a + 1111.2       ;  a - var * 2 + 837 / (x-3)   ")
    //printfn "%A" res
    //printfn "%s" (genExtCalc 50 10)
    //runExample()
    let getArg arg = 
        let ar = argv |> Array.choose (fun s -> 
            if s.StartsWith("-" + arg + "=") 
            then s.Substring(arg.Length + 2).Trim().Trim([| '"' |]).Trim() |> Some 
            else None)
        if ar.Length = 1 then Some <| ar.[0] else None
    
    let run = getArg "run"
    let log = getArg "log"
    let test = getArg "test"
    let n = getArg "n"
    let ecpath = getArg "ecpath"

    let run = Some("cool-incr")in let test = None

    if run.IsSome && test.IsNone then 
        match run.Value with 
        | "incr" -> FsGll.Tests.Pure.Incremental.incrementalTest()
        | "cool-incr" -> FsGll.Tests.Pure.Incremental.coolIncrementalTest() 
        | _ -> printUsage()
    elif test.IsSome && run.IsNone then 
        measurePerformance test.Value n log ecpath 
    else printUsage()
    0