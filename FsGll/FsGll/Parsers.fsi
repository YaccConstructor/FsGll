module Parsers

[<Interface>]
type GParserResult = 
    abstract member IsSucc : bool 

type CharStream = 
    new : (string, int) -> CharStream

type GParser = 
    abstract member ChainG : Trampoline * CharStream -> (GParserResult -> unit) -> unit