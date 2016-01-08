module FsGll.TestingCommon

open FsGll.ExtCalcLexer

type E =
    | EM of E * char * E
    | EP of E * char * E
    | EUnary of Lexem * E
    | EVar of string
    | EVal of float
    | EAssign of E * E
    | EPgm of E list * E

let extCalcLexer s =
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString s
    Seq.initInfinite (fun _ -> FsGll.ExtCalcLexer.tokenize lexbuf)
    |> Seq.cache
    |> Seq.takeWhile (function | EOF -> false | _ -> true)
    |> Seq.toList