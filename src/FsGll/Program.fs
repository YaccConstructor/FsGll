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
    
[<EntryPoint>]
let main argv =
    printf "%s" (genExtCalc 50 10)
    //runExample ()
    printfn "Finished. Press any key..."
    Console.ReadLine() |> ignore
    0