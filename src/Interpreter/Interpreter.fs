module Interpreter.Interpreter

open Interpreter.Types
open Interpreter.Operations

let rec eval exp env =
    match exp with
    | App(e1, e2) -> apply (eval e1 env) (eval e2 env)
    | Bool(n) -> Bool(n)
    | Int(n) -> Int(n)
    | Float(n) -> Float(n)
    | List(n) -> List(List.map (fun x -> eval x env) n)
    | Var(x) -> Map.find x env
    | PrimitiveOperation(f) -> Op(f, ArgsNum f, [])
    | Cond(cond, yes, no) ->
        if Bool(true) = eval cond env then eval yes env else eval no env
    | Op(id, n, el) -> Op(id, n, el)
    | Let(id, e1, e2) ->
        let r = eval e1 env in
        eval e2 (Map.add id r env)
    | LetRec(id, e1, e2) ->
        eval e2 (Map.add id (RClosure(e1, env, id)) env)
    | Lam(param, body) -> Closure(exp, env)
    | Closure(exp, env) -> exp
    | Prog(exp_list) -> 
        if List.isEmpty exp_list then None
        else exp_list |> List.map (fun x -> eval x env) |> List.last
    | Print(x) -> 
        let res = eval x env
        match res with
        | Float(n) -> printfn "%A" n; None
        | Int(n) -> printfn "%A" n; None
        | Bool(n) -> printfn "%A" n; None
        | List(n) -> printfn "%A" n; None
        | _ -> printfn "the type does not support printing"; None
    | _ -> 
        printfn "invalid evaluation"
        exp

and apply e1 e2 = 
    match e1 with
    | Closure(Lam(param, body), env) -> 
        match e2 with
        | List(value_list) -> 
            let env_add = List.zip param value_list |> Map.ofList
            let new_env = Map.fold (fun acc key value -> Map.add key value acc) env env_add
            eval body new_env
    | RClosure(Lam(param, body), env, id) -> 
        match e2 with
        | List(value_list) -> 
            let env_add = List.zip param value_list |> Map.ofList
            let new_env = Map.fold (fun acc key value -> Map.add key value acc) env env_add
            eval body (Map.add id e1 new_env)
    | Op(id, n, args) ->
        if n = 1 then (Operation id) (args @ [e2])
        else Op(id, n - 1, args @ [e2])
