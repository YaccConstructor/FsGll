module FsGll.Tests

open FsGll.PureParsers
open NUnit.Framework

[<Test>]
let ``high `` () =
    let tok c = satisfy ((=)c)
    let digit = tok '0' |>> string
    let expr, exprRef = createParserForwardedToRef<_, string>("expr")
    exprRef :=  (pipe2 expr expr (+)) <|> (digit)
    let n = 6
    printfn "Input length: %A" n
    //let res = expr.Apply (new ArrInputStream<_>(String.replicate n "0", 0))
    //Assert.AreEqual(42,res)
