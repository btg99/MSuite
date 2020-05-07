module Interpreter

open System
open Parser

module Util =
    let withColor color f =
        Console.ForegroundColor <- color
        f ()
        Console.ResetColor()

    let getOr option d =
        match option with
        | Some x -> x
        | None   -> d

module World =
    type Objective = { data: Map<string, int>; displayName: string }

    type Function = Command list

    type Datapack = Map<string * string, Function>

    type World = { objectives: Map<string, Objective>; datapacks: Datapack list }

    let getObjective name world = Map.find name world.objectives

    let addObjective name displayName world = { world with objectives = Map.add name { data = Map.empty; displayName = displayName } world.objectives }

    let removeObjective name world = { world with objectives = Map.remove name world.objectives }

    let getScore playerName objectiveName world = (Map.find objectiveName world.objectives).data |> Map.find playerName

    let setScore playerName objectiveName score world =
        let objective = Map.find objectiveName world.objectives
        { world with objectives = Map.add objectiveName { objective with data = Map.add playerName score objective.data } world.objectives }

    let getFunction nspace name world = List.pick (Map.tryFind (nspace, name)) world.datapacks

open World
open Util

let mutable executedCount = 0

let rec interpret world writer cmd =
    let rec objectivesAdd (x: ObjectivesAdd) = 
        if Map.containsKey x.name world.objectives then
            (fun () -> fprintfn writer "An objective already exists by that name") |> withColor ConsoleColor.Red 
            world
        else
            let displayName = getOr x.displayName x.name
            fprintfn writer "Created new objective [%s]" displayName
            addObjective x.name displayName world

    and displayObjective x = "[" + x.displayName + "]"

    and seperate delimiter source =
        match source with
        | x::y::rest -> x + delimiter + seperate delimiter (y::rest)
        | [x]        -> x
        | []         -> ""

    and objectivesList () =
        if world.objectives.Count > 0 then
            fprintfn writer "There are %i objectives: %s" world.objectives.Count (world.objectives |> Map.toList |> List.map snd |> List.map displayObjective |> seperate ", ")
        else
            fprintfn writer "There are no objectives"
        world

    and objectivesModify (m: ObjectivesModify) =
        fprintfn writer "Changed objective %s display name to [%s]" m.objective m.displayName
        addObjective m.objective m.displayName world

    and objectivesRemove (r: ObjectivesRemove) =
        fprintfn writer "Removed objective %s" (Map.find r.objective world.objectives |> displayObjective)
        removeObjective r.objective world

    and objectives o =
        match o with
        | Objectives.Add a    -> objectivesAdd a
        | List                -> objectivesList ()
        | Modify m            -> objectivesModify m
        | Objectives.Remove r -> objectivesRemove r

    and playersAdd (a: PlayersAdd) =
        let (Name name) = a.target
        let objective = getObjective a.objective world
        let oldScore = getOr (Map.tryFind name objective.data) 0
        let newScore = oldScore + a.score 
        fprintfn writer "Added %i to %s for %s (now %i)" a.score (displayObjective objective) name newScore
        setScore name a.objective newScore world 

    and playersRemove (x: PlayersRemove) =
        let (Name name) = x.target
        let objective = getObjective x.objective world
        let oldScore = getOr (Map.tryFind name objective.data) 0
        let newScore = oldScore - x.score 
        fprintfn writer "Removed %i from %s for %s (now %i)" x.score (displayObjective objective) name newScore
        setScore name x.objective newScore world 

    and playersGet (x: PlayersGet) =
        let (Name name) = x.target
        let objective = getObjective x.objective world
        let score = Map.find name objective.data
        fprintfn writer "%s has %i %s" name score (displayObjective objective)
        world

    and playersSet (x: PlayersSet) =
        let (Name name) = x.target
        let objective = getObjective x.objective world
        fprintfn writer "Set %s for %s to %i" (displayObjective objective) name x.score
        setScore name x.objective x.score world

    and operation x =
        match x with
        | PlusEqual -> (+)

    and playersOperation (x: PlayersOperation) =
        let (Name targetName) = x.target
        let (Name sourceName) = x.source
        let op = operation x.operation
        let targetScore = getScore targetName x.targetObjective world
        let sourceScore = getScore sourceName x.sourceObjective world
        let score = op targetScore sourceScore
        fprintfn writer "Set %s for %s to %i" (getObjective x.targetObjective world |> displayObjective) targetName score
        setScore targetName x.targetObjective score world

    and players p =
        match p with
        | Players.Add x    -> playersAdd x
        | Players.Remove x -> playersRemove x
        | Get x            -> playersGet x
        | Set x            -> playersSet x
        | Operation x      -> playersOperation x

    and scoreboard s =
        match s with
        | Objectives o -> objectives o
        | Players p    -> players p

    and func (x: Parser.Function) =
        let before = executedCount
        let f = getFunction x.nspace x.name world
        let world' = List.fold (fun w c -> interpret w writer c) world f
        fprintfn writer "Executed %i commands from function '%s:%s'" (executedCount - before) x.nspace x.name
        world'

    and comparisonOperator x =
        match x with
        | LessThan -> (<)
        | GreaterThan -> (>)

    and comparison x =
        let (Name tName) = x.target
        let (Name sName) = x.source
        let a = getScore tName x.targetObjective world
        let b = getScore sName x.sourceObjective world
        let comp = comparisonOperator x.comparisonOperator
        if comp a b then
            fprintfn writer "Test passed"
            match x.nextExecute with
            | Some e -> execute e
            | None   -> world
        else
            (fun () -> fprintfn writer "Test failed") |> withColor ConsoleColor.Red
            world

    and score s =
        match s with
        | Comparison x -> comparison x 

    and executeIf i =
        match i with
        | Score x -> score x

    and execute e =
        match e with
        | If x -> executeIf x
        | Run x -> command x

    and error m =
        (fun () -> fprintfn writer "%s" m) |> withColor ConsoleColor.Red
        world

    and command c =
        match c with
        | Scoreboard s -> scoreboard s
        | Function x   -> func x
        | Execute x    -> execute x
        | Error m      -> error m
        | NOP          -> world

    executedCount <- executedCount + 1
    command cmd