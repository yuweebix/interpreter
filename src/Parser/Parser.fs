module Parser.Parser

open FParsec
open Interpreter.Types

let (Parse: Parser<expr, obj> , ParseRef) = createParserForwardedToRef()

