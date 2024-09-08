module Parser.Conditions

open FParsec
open Interpreter.Types
open Parser.Parser

let Cond =
    (pstring "if" >>. spaces >>. Parse .>> spaces) .>>.
    (skipChar '{' >>. spaces >>. many Parse .>> spaces .>> skipChar ';' .>> spaces .>> skipChar '}' .>> spaces) .>>.
    (pstring "else" >>. spaces >>. skipChar '{' >>. spaces >>. many Parse .>> spaces .>> skipChar ';' .>> spaces .>> skipChar '}' .>> spaces) |>>
    fun ((cond, res), alt) -> Cond(cond, Prog(res), Prog(alt))
