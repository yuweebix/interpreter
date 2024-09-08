module Main.Init

open FParsec

open Parser.Parser
open Parser.Expressions
open Parser.Functions
open Parser.Variables
open Parser.Conditions
open Parser.Operations

ParseRef := choice [
    Function
    ParsePrint
    Cond
    attempt Operation
    attempt FunCall
    attempt IndexList
    attempt Method
    Var
    ParseList
    ParseFloat
    ParseBool
    Name
]

let MainParse = Parse