(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
FsGll
======================

A parser combinator library based on the GLL algorithm for F#.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FsGll library can be <a href="https://nuget.org/packages/FsGll">installed from NuGet</a>:
      <pre>PM> Install-Package FsGll</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

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
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/YaccConstructor/FsGll/tree/master/docs/content
  [gh]: https://github.com/YaccConstructor/FsGll
  [issues]: https://github.com/YaccConstructor/FsGll/issues
  [readme]: https://github.com/YaccConstructor/FsGll/blob/master/README.md
  [license]: https://github.com/YaccConstructor/FsGll/blob/master/LICENSE.txt
*)
