//
// All source code herein is Copyright (C) 2014, Philip Davis
//
// Robot Turtles is among the trademarks of Robot Turtles LLC, used here by permission
// Thanks to Dan Shapiro and all the Kickstarter backers for a great game.
//

namespace RobotTurtles

open RobotTurtles.Game

type SolveResult =
    | Done of Card list
    | NoSolution

//
// I wrote this naive solver first just to get something working.
// This is not intended to be used at all... it's really just here for demonstration purposes.
//
//
// SCROLL DOWN to see a more efficient solver.
//
//
type NaiveBreadthFirstSolver =
    static member public Solve (board: Board) (color: Color) =
        // Helper methods to make code more readable
        let wasValidMove = (fun (moveResult, nextState) -> moveResult <> Bug)
        let wasWinningMove = (fun (moveResult, nextState) -> moveResult = Win)

        //
        // Take a list of game states to evaluate
        // Returns NoSolution or the shortest list of cards that result in a win
        //
        let rec breadthFirstSearch n (stateQueue: Board list) =
            match stateQueue with
            | [] ->
                (n, NoSolution) // There are no more possible states to test... so there is no solution

            | state :: tail ->
                // Helper method to remove certain card permutations to limit the search space
                let notOptimizedOut =
                    (fun card ->
                        match (card, state.GetPlayedCards color |> List.rev) with
                        // Turning one direction and then turning back is pointless
                        | (TurnRight, TurnLeft :: _) -> false 
                        | (TurnLeft, TurnRight :: _) -> false
                        
                        // Turning 270 is pointless (could just turn -90 degrees instead)
                        | (TurnRight, TurnRight :: TurnRight :: _) -> false
                        | (TurnLeft, TurnLeft :: TurnLeft :: _) -> false

                        // Disallow firing lasers on consecutive turns
                        // Note: it's perfectly legit to fire a laser twice in a row... but disallowing
                        //       it doesn't affect the shortest possible solution (in a one-player game (without function frog))
                        //       for instance, player could [FireLaser; MoveForward; FireLaser]
                        | (FireLaser, FireLaser :: _) -> false

                        // Allow the move if we have no rules to block it
                        | _ -> true
                    )

                // Play each possible card for the given state
                // Filter out the invalid moves
                let moves =
                    state.GetPossibleCards color
                        |> List.filter notOptimizedOut
                        |> List.map (fun card -> state.Move color card)
                        |> List.filter wasValidMove

                // Check all the possible moves for a win
                match moves |> List.tryFind wasWinningMove with
                | Some (_, winningState) ->
                    // Found a winning move... return the sequence of played cards
                    (n, Done (winningState.GetPlayedCards color))

                | None ->
                    // No winning moves found.
                    // Continue searching... append the next states to the queue
                    breadthFirstSearch (n + 1) (tail @ (moves |> List.map (fun (_, nextState) -> nextState)))

        breadthFirstSearch 1 (board :: [])



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//
// IDAStarSolver performs an iterative deepening A* search to find a shortest path
// (not including function frog). This is much faster than naive breadth-first...
// However, it's still possible to take a very long time if boxes are placed in
// such a way to cause the heuristic function to become useless.
//

type IDAInterimResult =
    | Found of Board
    | NotFound
    | Cost of int

type IDAStarSolver =
    static member public Solve (board: Board) (color: Color) =
        // Helper method to make code more readable
        let wasValidMove = (fun (moveResult, nextState) -> moveResult <> Bug)

        // Cache the position of the gem we're looking for
        // Note: official rules say a turtle can collect any colour gem for the win.
        //       I've chosen to ignore that here... colours need to match.
        let (gemX, gemY) =
            match board.LocateGem color with
            | Some location -> location
            | _ -> invalidArg "color" (sprintf "%A gem is not on the board" color)

        let idaStar rootState calculateCost isGoal h =
            // search performs a depth-first search that stops at the specified depth
            let rec search n (state: Board) g bound =
                // Helper method to remove certain card permutations to limit the search space
                // Note: on its own, this method is poorly named... but it makes sense when you
                //       look at it being used (e.g. nextStates |> List.filter notOptimizedOut)
                //       I'd rather use a non-negated name but there is no List.filterNot like in Scala.
                let notOptimizedOut =
                    (fun card ->
                        match (card, state.GetPlayedCards color |> List.rev) with
                        // Turning one direction and then turning back is pointless
                        | (TurnRight, TurnLeft :: _) -> false 
                        | (TurnLeft, TurnRight :: _) -> false
                        
                        // Turning 270 is pointless (could just turn -90 degrees instead)
                        | (TurnRight, TurnRight :: TurnRight :: _) -> false
                        | (TurnLeft, TurnLeft :: TurnLeft :: _) -> false

                        // Disallow firing lasers on consecutive turns
                        // Note: it's perfectly legit to fire a laser twice in a row... but disallowing
                        //       it doesn't affect the shortest possible solution (in a one-player game)
                        | (FireLaser, FireLaser :: _) -> false

                        // Allow the move if we have no rules to block it
                        | _ -> true
                    )

                // Play each possible card for the given state
                // Filter out the invalid moves
                let calculateNextStates =
                    state.GetPossibleCards color
                        |> List.filter notOptimizedOut
                        |> List.map (fun card -> state.Move color card)
                        |> List.filter wasValidMove
                        |> List.map (fun (moveResult, nextState) -> nextState)

                // Calculate the estimated cost of reaching the goal state
                // (g = observed cost from start to here; h = estimated cost from here to goal)
                let f = g + h(state)

                // Bail out if the cost exceeds the current bound
                if f > bound then
                    (n + 1, Cost f)

                // Bail out if we found the goal state
                else if isGoal state then
                    (n + 1, Found state)

                else
                    let rec checkNextStates n min nextStates =
                        match nextStates with
                        | [] -> (n, Cost min) // No more states to evaluate. Return the cheapest cost found so far.
                        | nextState :: tail ->
                            // Depth first search on the next state
                            let interimResult =
                                search n nextState (g + (calculateCost state nextState)) bound

                            match interimResult with
                            | (_, Found(_)) as foundResult -> foundResult
                            | (n, Cost t) -> checkNextStates (n + 1) (if t < min then t else min) tail
                            | _ -> checkNextStates (n + 1) min tail

                    checkNextStates n System.Int32.MaxValue calculateNextStates

            let rec searchCurrentDepth n bound =
                match search n rootState 0 bound with
                | (_, Found(_)) as foundResult -> foundResult
                | (n, Cost t) when t = System.Int32.MaxValue -> (n, NotFound)
                | (n, Cost newBound) -> searchCurrentDepth n newBound
                | _ -> invalidArg "state" "Unexpected state"

            // Begin the IDA* search
            searchCurrentDepth 0 (h rootState)


        //
        // Helper method to calculate the cost of transitioning from state to nextState
        //
        let calculateCost state nextState =
            1 // Cost is always one because it always costs 1 card to transition states
              // Note, I haven't implemented function frog. But I think I'd still keep
              // the cost here at 1 and deal with the subroutine cards elsewhere.

        //
        // Helper method to determine if the specified state is a goal state.
        // In our case, the goal state occurs when the turtle's coordinates
        // match the cached coordinates for the gem of the same colour.
        // Note: official game rules say that any colour gem will win the game.
        //
        let isGoal (state: Board) =
            match state.GetTurtle color with
            | Some (turtleX, turtleY, _) when turtleX = gemX && turtleY = gemY -> true
            | _ -> false

        //
        // h is the A* heuristic function that estimates the cost from the current state
        // to the goal state. Since h is not allowed to overestimate the true cost,
        // we simply sum the difference in rows and difference in columns and then add
        // a card for the 90degree rotation (if applicable).
        //
        let h (state: Board) =
            match state.GetTurtle color with
            | Some (turtleX, turtleY, _) ->
                let xDelta = System.Math.Abs(turtleX - gemX)
                let yDelta = System.Math.Abs(turtleY - gemY)
                // Account for a turning card if the columns or rows are different
                let turnDelta = if xDelta > 0 && yDelta > 0 then 1 else 0
                xDelta + yDelta + turnDelta

            | _ -> invalidArg "color" (sprintf "%A turtle is not on the board" color)

        //
        // Initialize IDA* algorithm with the initial state
        // and translate the IDAInterimResult to a SolveResult
        //
        match idaStar board calculateCost isGoal h with
        | (n, Found endState) -> (n, Done (endState.GetPlayedCards color))
        | (n, _) -> (n, NoSolution)
