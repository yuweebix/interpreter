module Parser.Operations

open FParsec
open Interpreter.Types
open Parser.Parser
open Parser.Expressions
open Parser.Functions

let Operator = choice[
    pstring "&" |>> id
    pstring "|" |>> id
    pstring "not" |>> id
    pstring "+" |>> id
    pstring "-" |>> id
    pstring "*" |>> id
    pstring "/" |>> id
    pstring "=" |>> id
    pstring "!=" |>> id
    pstring ">" |>> id
    pstring "<" |>> id
    pstring ">=" |>> id
    pstring "<=" |>> id
]

let Operation = 
    (ParseFloat <|> attempt FunCall <|> Name <|> ParseBool) .>> spaces .>>. 
    Operator .>> spaces .>>. 
    (ParseFloat <|> attempt FunCall <|> Name <|> ParseBool) .>> spaces |>> 
        fun ((e1, op), e2) -> 
            App(App(PrimitiveOperation(op),  e1),  e2)

let IndexList = 
    Name .>>.
    (spaces >>. skipChar '[' >>. spaces >>. pint32 .>> spaces .>> skipChar ']' .>> spaces .>> skipChar ';' .>> spaces) |>>
        fun(list, num) -> 
            App(App(PrimitiveOperation("[]"), list), Int(num))

let Method = 
    Name .>> spaces .>>.
    (skipChar '.' >>. Name .>> spaces) .>>.
    (skipChar '(' >>. many Parse .>> spaces .>> skipChar ')' .>> spaces .>> skipChar ';' .>> spaces ) |>>
        fun((list, Var(method)), args) -> 
            let acc = App(PrimitiveOperation(method), list)
            List.fold (fun acc x -> App(acc, x)) acc args 

let ParsePrint =
    pstring "print" >>. spaces >>. skipChar '(' >>. spaces >>. many Parse .>> spaces .>> skipChar ')' .>> spaces .>> skipChar ';' .>> spaces |>> fun(arg) -> Print(Prog(arg))
