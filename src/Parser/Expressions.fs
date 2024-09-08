module Parser.Expressions

open FParsec
open Interpreter.Types
open Parser.Parser

let ParseFloat: Parser<expr, obj> = pfloat .>> spaces |>> Float

let ParseBool: Parser<expr, obj> =
    pstring "true" <|> pstring "false" .>> spaces |>> 
        function
            | "true" -> Bool(true)
            | "false" -> Bool(false)

let Name: Parser<expr, obj> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "invalid variable or function name" .>> spaces |>> Var

let ParseList = 
    skipChar '[' >>. spaces >>. sepBy (Parse .>> spaces) (skipChar ';' >>. spaces) .>> skipChar ']' .>> spaces |>> List
