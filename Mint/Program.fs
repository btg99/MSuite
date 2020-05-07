module Program

open System
open System.IO
open Lexer
open Parser
open Interpreter

let intro () = printfn "Minecraft Interpreter (Mint)"

let rec repl world =
    printf "/"
    Console.ReadLine() |> Seq.toList
                       |> lex
                       |> parse
                       |> interpret world Console.Out
                       |> repl

let runRepl () =
    intro ()
    repl { objectives = Map.empty; datapacks = [] }
    0

let entry nspace functionName (fileContents: string) =
    let commands = fileContents.Split([|"\n"; "\r\n"|], StringSplitOptions.None)
                        |> List.ofArray
                        |> List.map (Seq.toList >> lex >> parse)
    ((nspace, functionName), commands)

let getPack path =
    Directory.GetFiles(path, "*.mcfunction") |> List.ofArray
                                             |> List.map (fun funcPath -> entry (Path.GetFileName path) (Path.GetFileNameWithoutExtension funcPath) (File.ReadAllText funcPath))
                                             |> Map.ofList

let runPack (path: string) =
    let nspace = Path.GetFileNameWithoutExtension path
    let datapack = getPack path
    sprintf "function %s:main" nspace |> Seq.toList
                                      |> lex
                                      |> parse
                                      |> interpret { objectives = Map.empty; datapacks = [datapack] } Console.Out
                                      |> ignore
    0

[<EntryPoint>]
let main argv =
    match argv with
    | [||]     -> runRepl ()
    | [|path|] -> runPack path
    | _        -> 1
