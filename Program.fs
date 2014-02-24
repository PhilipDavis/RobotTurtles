//
// All source code herein is Copyright (C) 2014, Philip Davis
//
// Robot Turtles is among the trademarks of Robot Turtles LLC, used here by permission
// Thanks to Dan Shapiro and all the Kickstarter backers for a great game.
//

namespace RobotTurtles

open System
open RobotTurtles.Game

module Program =

    let color = Color.Red

    // TODO: translate these old tests into GameMaker format
    (*
    // In The Deep #3
    let board =
        Board.Initialize(
            { X = 1; Y = 0; Piece = (Some StoneWall) } ::
            { X = 2; Y = 0; Piece = (Some StoneWall) } ::
            { X = 3; Y = 0; Piece = (Some StoneWall) } ::
            { X = 4; Y = 0; Piece = (Some StoneWall) } ::
            { X = 5; Y = 0; Piece = (Some StoneWall) } ::
            { X = 1; Y = 1; Piece = (Some StoneWall) } ::
            { X = 2; Y = 1; Piece = (Some StoneWall) } ::
            { X = 3; Y = 1; Piece = (Some (Gem(color))) } ::
            { X = 4; Y = 1; Piece = (Some StoneWall) } ::
            { X = 5; Y = 1; Piece = (Some StoneWall) } ::
            { X = 1; Y = 2; Piece = (Some StoneWall) } ::
            { X = 2; Y = 2; Piece = (Some StoneWall) } ::
            { X = 3; Y = 2; Piece = (Some IceWall) } ::
            { X = 4; Y = 2; Piece = (Some StoneWall) } ::
            { X = 5; Y = 2; Piece = (Some StoneWall) } ::
            { X = 1; Y = 3; Piece = (Some StoneWall) } ::
            { X = 2; Y = 3; Piece = (Some StoneWall) } ::
            { X = 4; Y = 3; Piece = (Some StoneWall) } ::
            { X = 5; Y = 3; Piece = (Some StoneWall) } ::
            { X = 1; Y = 4; Piece = (Some StoneWall) } ::
            { X = 5; Y = 4; Piece = (Some StoneWall) } ::
            { X = 2; Y = 5; Piece = (Some Box) } ::
            { X = 3; Y = 5; Piece = (Some Box) } ::
            { X = 4; Y = 5; Piece = (Some Box) } ::
            { X = 3; Y = 7; Piece = (Some (Turtle(color, North))) } ::
            []
        )
    *)
    (*
    // Four Boxes // this one is slow... about 50s on my machine.
    let board =
        Board.Initialize(
            { X = 4; Y = 0; Piece = (Some StoneWall) } ::
            { X = 4; Y = 1; Piece = (Some StoneWall) } ::
            { X = 6; Y = 1; Piece = (Some (Gem(color))) } ::
            { X = 5; Y = 2; Piece = (Some Box) } ::
            { X = 6; Y = 2; Piece = (Some Box) } ::
            { X = 4; Y = 3; Piece = (Some Box) } ::
            { X = 5; Y = 3; Piece = (Some Box) } ::
            { X = 7; Y = 3; Piece = (Some StoneWall) } ::
            { X = 4; Y = 4; Piece = (Some IceWall) } ::
            { X = 5; Y = 4; Piece = (Some StoneWall) } ::
            { X = 7; Y = 7; Piece = (Some (Turtle(color, North))) } ::
            []
        )
    *)
    (*
    // Three Terrible Traps #2 // 70s
    let board =
        Board.Initialize(
            { X = 0; Y = 0; Piece = (Some (Gem(color))) } ::
            { X = 3; Y = 0; Piece = (Some IceWall) } ::
            { X = 4; Y = 0; Piece = (Some StoneWall) } ::
            { X = 0; Y = 1; Piece = (Some IceWall) } ::
            { X = 1; Y = 1; Piece = (Some Box) } ::
            { X = 4; Y = 1; Piece = (Some StoneWall) } ::
            { X = 0; Y = 2; Piece = (Some StoneWall) } ::
            { X = 2; Y = 2; Piece = (Some Box) } ::
            { X = 3; Y = 2; Piece = (Some StoneWall) } ::
            { X = 4; Y = 2; Piece = (Some StoneWall) } ::
            { X = 0; Y = 3; Piece = (Some StoneWall) } ::
            { X = 1; Y = 3; Piece = (Some StoneWall) } ::
            { X = 4; Y = 3; Piece = (Some StoneWall) } ::
            { X = 0; Y = 4; Piece = (Some StoneWall) } ::
            { X = 4; Y = 4; Piece = (Some StoneWall) } ::
            { X = 0; Y = 5; Piece = (Some StoneWall) } ::
            { X = 1; Y = 5; Piece = (Some Box) } ::
            { X = 2; Y = 5; Piece = (Some Box) } ::
            { X = 3; Y = 5; Piece = (Some Box) } ::
            { X = 4; Y = 5; Piece = (Some StoneWall) } ::
            { X = 2; Y = 7; Piece = (Some (Turtle(color, North))) } ::
            []
        )
    *)

    (*
    // Ouch... this takes > 500s to solve on my i7
    let board =
        GameMaker.Make [|
            "Bv            P<";
            "                ";
            "    ######      ";
            "    ##Rg##      ";
            "    ii[]ii      ";
            "        ##      ";
            "                ";
            "R>            G^"
        |]
    *)
    let board =
        GameMaker.Make [|
            "Bv            P<";
            "                ";
            "    ##ii####    ";
            "    ##Rg  ##    ";
            "    ##    ii    ";
            "    ##[][]##    ";
            "                ";
            "R>            G^"
        |]


    let startTime = Environment.TickCount
    
    let result =
        IDAStarSolver.Solve board color

    let stopTime = Environment.TickCount

    match result with
    | (n, NoSolution) -> printfn "No Solution\n%A steps, %Ams" n (stopTime - startTime)
    | (n, Done(cards)) ->
        printfn "Found solution %A\n%A steps, %Ams" cards n (stopTime - startTime)

    printfn ""
