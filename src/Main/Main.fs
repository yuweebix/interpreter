module Main.Main

open System.IO
open FParsec
open Interpreter.Types
open Interpreter.Interpreter
open Main.Init

let final = spaces >>. many MainParse .>> eof |>> Prog

let readFile filePath =
    try
        File.ReadAllText(filePath)
    with
    | :? FileNotFoundException ->
        failwithf "File not found: %s" filePath
    | ex ->
        failwithf "An error occurred while reading the file: %s" ex.Message

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "ERROR: please provide one argument - name of the file containing the code"
    else
        let filename = argv.[0]
        
        if File.Exists(filename) then
            let content = readFile filename
            match runParserOnString final () "" content with
            | Success(result,_,_) -> 
                eval result Map.empty |> ignore
            | Failure(err,_,_) -> printfn "%A" err
        else
            printfn "ERROR: File '%s' does not exist." filename
    0
