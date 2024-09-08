module Parser.Functions

open FParsec
open Interpreter.Types
open Parser.Parser
open Parser.Expressions

let Function =
    (pstring "fun" >>. spaces >>. Name) .>>. 
    (skipChar '(' >>. spaces >>. sepBy (Name .>> spaces) (skipChar ';' >>. spaces) .>> skipChar ')') .>>. 
    (spaces >>. skipChar '{' >>. spaces >>. many Parse .>> spaces .>> skipChar ';' .>> spaces .>> skipChar '}' .>> spaces) .>>. 
    (many Parse .>> spaces) |>> 
        fun(((Var(func), params), body), expr_list) -> 
            let ids_extracted = 
                params |> List.map(
                    function
                        | Var(x) -> x 
            )

            LetRec(func, Lam(ids_extracted, Prog(body)), Prog(expr_list))

let FunCall =
    Name .>>. 
    (spaces >>. skipChar '(' >>. spaces >>. sepBy (Parse .>> spaces) (skipChar ';' >>. spaces) .>> skipChar ')' .>> spaces .>> skipChar ';' .>> spaces) |>>
        fun(func, args) -> App(func, List(args))
