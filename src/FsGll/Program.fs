module FsGll.Program

open System
open System.Collections
open System.Collections.Generic
//open Parsers
//open Parsersok
open FSharpx.Prelude
open System.Diagnostics
open Tests3
           
//type MyData<'a> = 
//    | Constr of 'a
//    | Nothing

//let hand (a: MyData<#Object>) =
//    let s = a :> MyData<Object>
//    ()

//let a1 = Constr("aaaa")
//let a2 = Constr(new Object())
//hand a1
                 
    
[<EntryPoint>]
let main argv =
    Tests.Incremental.incrementalTest()
//    let res = TestsPure.runExtCalc("a = 0; var = a + 1111.2       ;  a - var * 2 + 837 / (x-3)   ")
//    printfn "%A" res
    failwith "here"
    //printfn "%s" (genExtCalc 50 10)
    //runExample()
    let getArg arg = 
        let ar = argv |> Array.choose (fun s -> 
            if s.StartsWith("-" + arg + "=") 
            then s.Substring(arg.Length + 2).Trim().Trim([| '"' |]).Trim() |> Some 
            else None)
        if ar.Length = 1 then Some <| ar.[0] else None

    let log = getArg "log"
    let test = getArg "test"
    let n = getArg "n"
    let ecpath = getArg "ecpath"
    if test.IsSome then 
        measurePerformance test.Value n log ecpath 
    else 
        //runExample ()
        printfn "Usage: <pgm> -test=<Test> [-n=<N>] [-log=<Log>] [-ecpath=<ExtCalc tests dir path>]"
    0