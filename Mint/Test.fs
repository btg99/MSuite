module Test

open System
open System.IO
open NUnit.Framework

open Lexer
open Parser
open Interpreter

let rec private exec cmd world writer = 
    match cmd with
    | []    -> ()
    | x::xs -> x |> Seq.toList
                 |> lex
                 |> parse
                 |> interpret world writer
                 |> (fun w -> exec xs w writer)

let private run (input: string) (stream: Stream) =
    use writer = new StreamWriter(stream, leaveOpen = true)
    input.Split Environment.NewLine |> List.ofArray |> (fun cmd -> exec cmd { objectives = Map.empty; datapacks = [] } writer)

let private asString (stream: Stream) =
    stream.Seek(0L, SeekOrigin.Begin) |> ignore
    use reader = new StreamReader(stream, leaveOpen = true)
    reader.ReadToEnd()

let test input expected = 
    use stream = new MemoryStream()
    run input stream
    Assert.That(asString stream, Is.EqualTo(expected))

let private runPack pack (input: string) (stream: Stream) =
    use writer = new StreamWriter(stream, leaveOpen = true)
    let datapack = pack |> List.map (fun (key, commands: string) -> (key, commands.Split Environment.NewLine |> List.ofArray |> List.map (Seq.toList >> lex >> parse)))
                        |> Map.ofList
    input.Split Environment.NewLine |> List.ofArray |> (fun cmd -> exec cmd { objectives = Map.empty; datapacks = [datapack] } writer)

let testPack pack input expected =
    use stream = new MemoryStream()
    runPack pack input stream
    Assert.That(asString stream, Is.EqualTo(expected))


[<TestFixture>]
type ScoreboardTests () =

    [<Test>]
    member this.``same objective twice fails``() =
        test ("scoreboard objectives add obj dummy" + Environment.NewLine +
              "scoreboard objectives add obj dummy")
             ("Created new objective [obj]" + Environment.NewLine +
              "An objective already exists by that name" + Environment.NewLine)

    [<Test>]
    member this.``objective with display name``() =
        test ("scoreboard objectives add obj dummy \"fancy name\"")
             ("Created new objective [fancy name]" + Environment.NewLine)

    [<Test>]
    member this.``list no objectives``() =
        test ("scoreboard objectives list")
             ("There are no objectives" + Environment.NewLine)

    [<Test>]
    member this.``list one objective``() =
        test ("scoreboard objectives add obj dummy \"display name\"" + Environment.NewLine +
              "scoreboard objectives list")
             ("Created new objective [display name]" + Environment.NewLine +
              "There are 1 objectives: [display name]" + Environment.NewLine)

    [<Test>]
    member this.``list three objectives``() =
        test ("scoreboard objectives add obj1 dummy \"obj 1 name\"" + Environment.NewLine +
              "scoreboard objectives add obj2 dummy \"obj 2 name\"" + Environment.NewLine +
              "scoreboard objectives add obj3 dummy" + Environment.NewLine +
              "scoreboard objectives list")
             ("Created new objective [obj 1 name]" + Environment.NewLine +
              "Created new objective [obj 2 name]" + Environment.NewLine +
              "Created new objective [obj3]" + Environment.NewLine +
              "There are 3 objectives: [obj 1 name], [obj 2 name], [obj3]" + Environment.NewLine)

    [<Test>]
    member this.``modify displayName``() =
        test ("scoreboard objectives add obj dummy \"old name\"" + Environment.NewLine +
              "scoreboard objectives modify obj displayname \"new name\"" + Environment.NewLine +
              "scoreboard objectives list")
             ("Created new objective [old name]" + Environment.NewLine +
              "Changed objective obj display name to [new name]" + Environment.NewLine +
              "There are 1 objectives: [new name]" + Environment.NewLine)

    [<Test>]
    member this.``remove objective``() =
        test ("scoreboard objectives add obj dummy \"name\"" + Environment.NewLine +
              "scoreboard objectives remove obj" + Environment.NewLine +
              "scoreboard objectives list")
             ("Created new objective [name]" + Environment.NewLine +
              "Removed objective [name]" + Environment.NewLine +
              "There are no objectives" + Environment.NewLine) 

    [<Test>]
    member this.``add score for named player``() =
        test ("scoreboard objectives add obj dummy \"name\"" + Environment.NewLine +
              "scoreboard players add player1 obj 27" + Environment.NewLine +
              "scoreboard players add player1 obj 4")
             ("Created new objective [name]" + Environment.NewLine +
              "Added 27 to [name] for player1 (now 27)" + Environment.NewLine +
              "Added 4 to [name] for player1 (now 31)" + Environment.NewLine)

    [<Test>]
    member this.``remove score for named player``() =
        test ("scoreboard objectives add obj dummy \"name\"" + Environment.NewLine +
              "scoreboard players remove player1 obj 12" + Environment.NewLine +
              "scoreboard players remove player1 obj 11")
             ("Created new objective [name]" + Environment.NewLine +
              "Removed 12 from [name] for player1 (now -12)" + Environment.NewLine +
              "Removed 11 from [name] for player1 (now -23)" + Environment.NewLine) 
    
    [<Test>]
    member this.``get score for player``() =
        test ("scoreboard objectives add obj dummy \"name\"" + Environment.NewLine +
              "scoreboard players add player1 obj 24" + Environment.NewLine +
              "scoreboard players get player1 obj")
             ("Created new objective [name]" + Environment.NewLine +
              "Added 24 to [name] for player1 (now 24)" + Environment.NewLine +
              "player1 has 24 [name]" + Environment.NewLine)

    [<Test>]
    member this.``set score for player``() =
        test ("scoreboard objectives add obj dummy \"name\"" + Environment.NewLine +
              "scoreboard players set player1 obj -123" + Environment.NewLine +
              "scoreboard players get player1 obj")
             ("Created new objective [name]" + Environment.NewLine +
              "Set [name] for player1 to -123" + Environment.NewLine +
              "player1 has -123 [name]" + Environment.NewLine)

    [<Test>]
    member this.``operation plus equals``() =
        test ("scoreboard objectives add obj1 dummy \"obj1 name\"" + Environment.NewLine +
              "scoreboard objectives add obj2 dummy \"obj2 name\"" + Environment.NewLine +
              "scoreboard players set player1 obj1 15" + Environment.NewLine +
              "scoreboard players set player2 obj2 21" + Environment.NewLine +
              "scoreboard players operation player1 obj1 += player2 obj2" + Environment.NewLine +
              "scoreboard players get player1 obj1")
             ("Created new objective [obj1 name]" + Environment.NewLine +
              "Created new objective [obj2 name]" + Environment.NewLine +
              "Set [obj1 name] for player1 to 15" + Environment.NewLine +
              "Set [obj2 name] for player2 to 21" + Environment.NewLine +
              "Set [obj1 name] for player1 to 36" + Environment.NewLine +
              "player1 has 36 [obj1 name]" + Environment.NewLine)

[<TestFixture>]
type ExecuteTest () =

    [<Test>]
    member this.``execute if score less than``() =
        test ("scoreboard objectives add obj1 dummy" + Environment.NewLine +
              "scoreboard objectives add obj2 dummy" + Environment.NewLine +
              "scoreboard players set player1 obj1 1" + Environment.NewLine +
              "scoreboard players set player2 obj2 2" + Environment.NewLine +
              "execute if score player1 obj1 < player2 obj2" + Environment.NewLine +
              "execute if score player2 obj2 < player1 obj1")
             ("Created new objective [obj1]" + Environment.NewLine +
              "Created new objective [obj2]" + Environment.NewLine +
              "Set [obj1] for player1 to 1" + Environment.NewLine +
              "Set [obj2] for player2 to 2" + Environment.NewLine +
              "Test passed" + Environment.NewLine +
              "Test failed" + Environment.NewLine)

    [<Test>]
    member this.``execute if score less than then run``() =
        test ("scoreboard objectives add obj1 dummy" + Environment.NewLine +
              "scoreboard objectives add obj2 dummy" + Environment.NewLine +
              "scoreboard players set player1 obj1 1" + Environment.NewLine +
              "scoreboard players set player2 obj2 2" + Environment.NewLine +
              "execute if score player1 obj1 < player2 obj2 run scoreboard players set player1 obj1 -1" + Environment.NewLine +
              "execute if score player2 obj2 < player1 obj1 run scoreboard players set player2 obj2 -1")
             ("Created new objective [obj1]" + Environment.NewLine +
              "Created new objective [obj2]" + Environment.NewLine +
              "Set [obj1] for player1 to 1" + Environment.NewLine +
              "Set [obj2] for player2 to 2" + Environment.NewLine +
              "Test passed" + Environment.NewLine +
              "Set [obj1] for player1 to -1" + Environment.NewLine +
              "Test failed" + Environment.NewLine)

[<TestFixture>]
type FunctionTest () =
    
    [<Test>]
    member this.``basic function``() =
        testPack [(("myNamespace", "myFunction"), "scoreboard objectives add obj dummy" + Environment.NewLine +
                                                  "scoreboard objectives list")]
                 ("function myNamespace:myFunction")
                 ("Created new objective [obj]" + Environment.NewLine +
                  "There are 1 objectives: [obj]" + Environment.NewLine +
                  "Executed 2 commands from function 'myNamespace:myFunction'" + Environment.NewLine)

    [<Test>]
    member this.``recursive function``() =
        testPack [(("nspace", "start"),   "scoreboard objectives add obj dummy" + Environment.NewLine +
                                          "scoreboard players set 0 obj 0" + Environment.NewLine +
                                          "scoreboard players set x obj 3" + Environment.NewLine +
                                          "function nspace:recurse");
                  (("nspace", "recurse"), "scoreboard players remove x obj 1" + Environment.NewLine +
                                          "execute if score x obj > 0 obj run function nspace:recurse")]
                 ("function nspace:start")
                 ("Created new objective [obj]" + Environment.NewLine +
                  "Set [obj] for 0 to 0" + Environment.NewLine +
                  "Set [obj] for x to 3" + Environment.NewLine +
                  "Removed 1 from [obj] for x (now 2)" + Environment.NewLine +
                  "Test passed" + Environment.NewLine +
                  "Removed 1 from [obj] for x (now 1)" + Environment.NewLine +
                  "Test passed" + Environment.NewLine +
                  "Removed 1 from [obj] for x (now 0)" + Environment.NewLine +
                  "Test failed" + Environment.NewLine +
                  "Executed 2 commands from function 'nspace:recurse'" + Environment.NewLine +
                  "Executed 4 commands from function 'nspace:recurse'" + Environment.NewLine +
                  "Executed 6 commands from function 'nspace:recurse'" + Environment.NewLine +
                  "Executed 10 commands from function 'nspace:start'" + Environment.NewLine)