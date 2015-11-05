module Tests2
open System
open System.IO
open System.Diagnostics
open FSharpx.Prelude
open AbstractParsers

let tok c = satisfy ((=)c)
let str (s: string) = s |> Seq.fold (fun p c -> p .>> tok c) (preturn ())
let comma = tok ','
let whitespace = satisfy (fun c -> c = ' ' || c = '\n' || c = '\r' || c = '\t')
let digit = satisfy (Char.IsDigit)
let letter = satisfy (Char.IsLetter)

type JLexem = 
    | Brace1
    | Brace2
    | Bracket1
    | Bracket2
    | StrLit of string
    | HashKey of string
    | NumLit of int
    | Comma
    | Colon
    | BlockComment

let jlexemIsNum x = match x with NumLit _ -> true | _ -> false
let jlexemGetNum x = match x with NumLit x -> x | _ -> failwith ""

let jlexemIsStr x = match x with StrLit _ -> true | _ -> false
let jlexemGetStr x = match x with StrLit x -> x | _ -> failwith ""

let jlexemIsHashKey x = match x with HashKey _ -> true | _ -> false
let jlexemGetHashKey x = match x with HashKey x -> x | _ -> failwith ""

let runLexer (s: string) = 
    let number = 
        many whitespace >>. many1 digit .>> notFollowedBy digit 
        |>> (NumLit << int << String.concat "" << List.map string)

    let blockCommentElem = 
        satisfy ((<>)'*') 
        <|> (tok '*' >>. (satisfy ((<>)'/')))

    let blockComment = 
        many whitespace >>. 
        (str "/*" >>. many blockCommentElem .>> str "*/" |>> (konst BlockComment))
    let single c f =  many whitespace >>. tok c |>> (konst f)
    let strchr = 
            (str "\\\"" |>> konst '\"') 
        <|> (str "\\n"  |>> konst '\n') 
        <|> satisfy ((<>)'\"')

    let strlit = 
        many whitespace >>. (tok '"' >>. many strchr .>> tok '"') 
        |>> (StrLit << Seq.fold (+) "" << Seq.map string)

    let hashkey = 
        many whitespace >>. many1 letter .>> notFollowedBy letter
        |>> (HashKey << Seq.fold (+) "" << Seq.map string)

    let lexem = 
        strlit 
        <|> number
        <|> hashkey
        <|> single ':' Colon
        <|> single ',' Comma
        <|> single '{' Brace1
        <|> single '}' Brace2
        <|> single '[' Bracket1
        <|> single ']' Bracket2
        <|> blockComment
        <?> "Lexem expected"
    let s = s :> char seq

    let stream = many lexem .>> many whitespace
    let res = stream.Apply(new InputStream<_>(s, 0))
    
    res |> Seq.sortBy(fun x -> -x.Tail.Ind)  |> Seq.iter (printfn "%A")
    Seq.head res

let runLexer2 (s: string) = 
    let number = 
        many1 digit .>> notFollowedBy digit
        |>> (NumLit << int << String.concat "" << List.map string)

    let blockCommentElem = 
        satisfy ((<>)'*') 
        <|> (tok '*' >>. (satisfy ((<>)'/')))

    let blockComment = 
        (str "/*" >>. many blockCommentElem .>> str "*/" |>> (konst BlockComment))

    let single c f =  tok c |>> (konst f)
    let strchr = 
            (str "\\\"" |>> konst '\"') 
        <|> (str "\\n"  |>> konst '\n') 
        <|> satisfy ((<>)'\"')

    let strlit = 
        (tok '"' >>. many strchr .>> tok '"') 
        |>> (StrLit << Seq.fold (+) "" << Seq.map string)

    let hashkey = 
        many1 letter .>> notFollowedBy letter
        |>> (HashKey << Seq.fold (+) "" << Seq.map string)

    let lexem = 
        strlit 
        <|> number
        <|> hashkey
        <|> single ':' Colon
        <|> single ',' Comma
        <|> single '{' Brace1
        <|> single '}' Brace2
        <|> single '[' Bracket1
        <|> single ']' Bracket2
        <|> blockComment
        <?> "Lexem expected"
    let s = s :> char seq

    let stream = many whitespace >>. many (lexem .>> many whitespace)
    let res = stream.Apply(new InputStream<_>(s, 0))
    
    res |> Seq.sortBy(fun x -> -x.Tail.Ind)  |> Seq.iter (printfn "%A")
    Seq.head res

type JsonValue = 
    | JNull
    | JBool of bool
    | JNum of int
    | JString of string
    | JArray of JsonValue list
    | JObject of Map<string, JsonValue>

let jparser (inp: JLexem list) =
    let inp = inp |> List.filter (function BlockComment -> false | _ -> true)
    let jarray, jarrayRef = createParserForwardedToRef<_,_>("jvalue")
    let jvalue, jvalueRef = createParserForwardedToRef<_,_>("jvalue")
    let jobject, jobjectRef = createParserForwardedToRef<_,_>("jobject")

    let jnumber = satisfy jlexemIsNum |>> jlexemGetNum |>> JNum
    
    let jstring = satisfy jlexemIsStr |>> jlexemGetStr
    
    let jhashkey = satisfy jlexemIsHashKey |>> jlexemGetHashKey
    
    let jnull = satisfy (function HashKey s when s = "null" -> true | _ -> false) |>> konst JNull
    
    let jbool = 
            (satisfy (function HashKey s when s = "true" -> true | _ -> false) |>> konst (JBool true))
        <|> (satisfy (function HashKey s when s = "false" -> true | _ -> false) |>> konst (JBool false))

    jvalueRef :=
        jnumber 
        <|> jnull
        <|> jbool
        <|> (jstring |>> JString)
        <|> jarray
        <|> jobject
    
    jarrayRef := tok Bracket1 >>. sepBy jvalue (tok Comma) .>> tok Bracket2 |>> JArray
    
    let keyValueList = 
        sepBy (jhashkey <|> jstring .>>. ( tok Colon >>. jvalue)) (tok Comma)
        |>> Map.ofSeq
    
    jobjectRef := tok Brace1 >>. keyValueList .>> tok Brace2 |>> JObject
    
    let res = jvalue.Apply(new InputStream<_>(inp, 0))
    
    res |> Seq.sortBy(fun x -> -x.Tail.Ind) |> Seq.iter (printfn "%A")
    ()

let runExample () = 
    
    let sw = new Stopwatch()
    sw.Start()
    let res = runLexer2 (File.ReadAllText "jtest1.txt")
    printfn "Done, lexer time: %A" sw.Elapsed

    let sw = new Stopwatch()
    sw.Start()
    jparser ((res :?> GSuccess<char, JLexem list>).Value)
    printfn "Done, parser time: %A" sw.Elapsed
    ()


    
