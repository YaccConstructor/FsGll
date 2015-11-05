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
    runExample ()
    printfn "Finished. Press any key..."
    Console.ReadLine() |> ignore
    0