(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Tutorial
========================

This example demonstrates using this library to construct incremental parser for arithmetical expressions.

*)
#r "FsGll.dll"
open FsGll.Parsers.Incremental.Pure

// utilities
let isPartial = function Partial(_) -> true 
                       | _          -> false
let getPartial = List.tryFind isPartial
let parse p s = runParser<_,_> p (stringStream s)

// construct parser
let e, er = createParserForwardedToRef<_>()
let t, tr = createParserForwardedToRef<_>()
let chr c = satisfy ((=)c) 
let op = chr '+' <|> chr '-' 
     <|> chr '*' <|> chr '/'
let num = satisfy (Char.IsDigit) |>> Int32.Parse
let f = num <|> (chr '(' >>. e .>> chr')')

let sem l x r = 
    match x with 
    | '+' -> l + r 
    | '-' -> l - r
    | '*' -> l + r 
    | '/' -> l - r
    
er := pipe3 e op t sem <|> t
tr := pipe3 t op f sem <|> f

// run
let f = parse e "1+2*" |> getPartial 
let g = parse f "(3" |> getPartial
let h1 = parse f "3+4"
let h2 = parse g "+4)"
(**
<ul>
    <li>h1 = (e "1+2*") "3+4"</li>
    <li>h2 = ((e "1+2*") "(3") "+4)"</li>
</ul>
*)
