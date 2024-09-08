module Parser.Variables

open FParsec
open Interpreter.Types
open Parser.Parser
open Parser.Expressions

let Var = 
    pstring "let" >>. spaces >>. Name .>>. 
    (spaces >>. skipChar '=' >>. spaces >>. Parse) .>> spaces .>> skipChar ';' .>> spaces .>>. 
    many Parse .>> spaces |>> 
        fun ((Var(name), value), expr_list) -> Let(name, value, Prog(expr_list))
