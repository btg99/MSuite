module rec Parser

open Lexer

type Target = Name of string

type Criteria = Dummy
type ObjectivesAdd = { name: string; criteria: Criteria; displayName: string option }
type ObjectivesModify = { objective: string; displayName: string }
type ObjectivesRemove = { objective: string }
type Objectives =
    | Add of ObjectivesAdd
    | List
    | Modify of ObjectivesModify
    | Remove of ObjectivesRemove

type Operation =
    | PlusEqual

type PlayersAdd = { target: Target; objective: string; score: int }
type PlayersRemove = { target: Target; objective: string; score: int }
type PlayersGet = { target: Target; objective: string }
type PlayersSet = { target: Target; objective: string; score: int }
type PlayersOperation = { target: Target; targetObjective: string; operation: Operation; source: Target; sourceObjective: string }
type Players =
    | Add of PlayersAdd
    | Remove of PlayersRemove
    | Get of PlayersGet
    | Set of PlayersSet
    | Operation of PlayersOperation

type Scoreboard =
    | Objectives of Objectives
    | Players of Players

type Function = { nspace: string; name: string }

type ComparisonOperator =
    | LessThan
    | GreaterThan

type Comparison = {
    target: Target;
    targetObjective: string;
    comparisonOperator: ComparisonOperator;
    source: Target;
    sourceObjective: string;
    nextExecute: Execute option
}

type Score =
    | Comparison of Comparison

type If =
    | Score of Score

type Execute =
    | If of If
    | Run of Command

type Command =
    | Scoreboard of Scoreboard
    | Function of Function
    | Execute of Execute
    | Error of string
    | NOP

exception ParseError of string

let criteria x =
    match x with
    | "dummy" -> Dummy
    | _       -> failwith "expected criteria"

let objectivesAdd ts =
    match ts with
    | [Element name; Space; Element crit; End] -> { name = name; criteria = criteria crit; displayName = None }
    | [Element name; Space; Element crit; Space; RawText display; End] -> { name = name; criteria = criteria crit; displayName = Some display }
    | _ -> raise <| ParseError "'scoreboard objectives add' expects <objective> <criteria> [<display name>]"

let objectivesModify ts = 
    match ts with
    | [Element objective; Space; Element "displayname"; Space; RawText displayName; End] -> { objective = objective; displayName = displayName }
    | _ -> raise <| ParseError "'scoreboard objectives modify' expects <objective> displayname <displayName>"

let objectivesRemove ts =
    match ts with
    | [Element objective; End] -> { objective = objective }
    | _ -> raise <| ParseError "'scoreboard objectives remove' expects <objective>"

let objectives ts =
    match ts with
    | Element "add"::Space::xs    -> Objectives.Add <| objectivesAdd xs
    | [Element "list"; End]       -> List
    | Element "modify"::Space::xs -> Modify <| objectivesModify xs
    | Element "remove"::Space::xs -> Objectives.Remove <| objectivesRemove xs
    | _                           -> raise <| ParseError "'scoreboard objectives' expects subcommand add, list, or modify"

let playersAdd ts: PlayersAdd =
    match ts with
    | [Element target; Space; Element objective; Space; Element score; End] -> { target = Name target; objective = objective; score = int score }
    | _ -> raise <| ParseError "'scoreboard players add' expects <player> <objective> <score>"

let playersRemove ts: PlayersRemove =
    match ts with
    | [Element target; Space; Element objective; Space; Element score; End] -> { target = Name target; objective = objective; score = int score }
    | _ -> raise <| ParseError "'scoreboard players remove' expects <player> <objective> <score>"

let playersGet ts =
    match ts with
    | [Element target; Space; Element objective; End] -> { target = Name target; objective = objective }
    | _ -> raise <| ParseError "'scoreboard players get' expects <player> <objective>"

let playersSet ts =
    match ts with
    | [Element target; Space; Element objective; Space; Element score; End] -> { target = Name target; objective = objective; score = int score }
    | _ -> raise <| ParseError "'scoreboard players set' expects <player> <objective> <score>"

let toOperation sym =
    match sym with
    | Symbol.PlusEqual -> PlusEqual
    | _                -> raise <| ParseError "expected an operation"

let playersOperation ts =
    match ts with
    | [Element target; Space; Element targetObjective; Space; Symbol operation; Space; Element source; Space; Element sourceObjective; End] ->
        {
            target = Name target;
            targetObjective = targetObjective;
            operation = toOperation operation;
            source = Name source;
            sourceObjective = sourceObjective
        }
    | _ -> raise <| ParseError "'scoreboard players operation' expects <source player> <source objective> <operation> <target player> <target objective>"    

let players ts =
    match ts with
    | Element "add"::Space::xs       -> Players.Add <| playersAdd xs
    | Element "remove"::Space::xs    -> Players.Remove <| playersRemove xs
    | Element "get"::Space::xs       -> Get <| playersGet xs
    | Element "set"::Space::xs       -> Set <| playersSet xs
    | Element "operation"::Space::xs -> Operation <| playersOperation xs
    | _                              -> raise <| ParseError "'scoreboard players' expects subcommand add"

let scoreboard ts =
    match ts with
    | Element "objectives"::Space::xs -> Objectives <| objectives xs
    | Element "players"::Space::xs    -> Players <| players xs
    | _                               -> raise <| ParseError "'scoreboard' expects subcommand objectives or players"

let func ts = 
    match ts with
    | [Element nspace; Symbol Colon; Element name; End] -> { nspace = nspace; name = name }
    | _                                                  -> raise <| ParseError "'function' expectes <namespace>:<function>"

let toComparisonOperator sym =
    match sym with
    | Symbol.LessThan -> ComparisonOperator.LessThan
    | Symbol.GreaterThan -> ComparisonOperator.GreaterThan
    | _ -> raise <| ParseError "expected a comparison operator"

let score ts =
    match ts with
    | [Element target; Space; Element targetObjective; Space; Symbol cop; Space; Element source; Space; Element sourceObjective; End] ->
        Comparison {
            target = Name target;
            targetObjective = targetObjective;
            comparisonOperator = toComparisonOperator cop;
            source = Name source;
            sourceObjective = sourceObjective;
            nextExecute = None
        }
    | Element target::Space::Element targetObjective::Space::Symbol cop::Space::Element source::Space::Element sourceObjective::Space::rest ->
        Comparison {
            target = Name target;
            targetObjective = targetObjective;
            comparisonOperator = toComparisonOperator cop;
            source = Name source;
            sourceObjective = sourceObjective;
            nextExecute = Some (execute rest)
        }
    | _ -> raise <| ParseError "'execute if score' expects <target> <targetObjective> ..."

let eIf ts =
    match ts with
    | Element "score"::Space::xs -> Score <| score xs
    | _                          -> raise <| ParseError "'execute if' expects score"

let execute ts =
    match ts with
    | Element "if"::Space::xs -> If <| eIf xs
    | Element "run"::Space::xs -> Run <| command xs
    | _                       -> raise <| ParseError "execute expects if or run"

let command ts =
    match ts with
    | Element "scoreboard"::Space::xs -> Scoreboard <| scoreboard xs
    | Element "function"::Space::xs   -> Function <| func xs
    | Element "execute"::Space::xs    -> Execute <| execute xs
    | _                               -> raise <| ParseError "expected command scoreboard or function"

let parse tokens =
    match tokens with
    | End::_ | [] -> NOP
    | _           ->
        try 
            command tokens
        with
        | ParseError m -> Error m 