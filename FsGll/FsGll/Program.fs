open System
open System.Collections
open System.Collections.Generic
//open Parsers
//open Parsersok
open FSharpx.Prelude
open System.Diagnostics
open Tests2
                 
[<EntryPoint>]
let main argv =
    runExample ()
    Console.ReadLine() |> ignore
    0