module Interpreter.Operations

open Interpreter.Types
let ArgsNum = function
| "&" | "|" | "+" | "-" | "*" | "/" | "=" | "!=" | ">" | "<" | ">=" | "<=" -> 2
| "!" | "front" | "back" -> 1
| "[]" | "append" -> 2

let Operation = function
| "[]" -> function
    | [List(list); Int(n)] -> list.[n] 
| "front" -> function
    | [List(list)] -> List.head list
| "back" -> function
    | [List(list)] -> List.last list
| "append" -> function
    | [List(list); x] -> List(list @ [x])
| "&" -> function 
    | [Bool(a); Bool(b)] -> Bool(a && b)
| "|" -> function 
    | [Bool(a); Bool(b)] -> Bool(a || b)
| "not" -> function 
    | [Bool(a)] -> Bool(not a)
| "+" -> function 
    | [Float(a); Float(b)] -> Float(a + b)
| "-" -> function 
    | [Float(a); Float(b)] -> Float(a - b)
| "*" -> function 
    | [Float(a); Float(b)] -> Float(a * b)
| "/" -> function 
    | [Float(a); Float(b)] -> Float(a / b)
| ">" -> function 
    | [Float(a); Float(b)] -> Bool(a > b)
| "<" -> function 
    | [Float(a); Float(b)] -> Bool(a < b)
| ">=" -> function 
    | [Float(a); Float(b)] -> Bool(a >= b)
| "<=" -> function 
    | [Float(a); Float(b)] -> Bool(a <= b)
| "=" -> function 
    | [Float(a); Float(b)] -> Bool(a = b)
| "!=" -> function 
    | [Float(a); Float(b)] -> Bool(a <> b)
