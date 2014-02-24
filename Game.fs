//
// All source code herein is Copyright (C) 2014, Philip Davis
//
// Robot Turtles is among the trademarks of Robot Turtles LLC, used here by permission
// Thanks to Dan Shapiro and all the Kickstarter backers for a great game.
//

namespace RobotTurtles.Game

type Direction =
    | North
    | East
    | South
    | West

type Color =
    | Blue = 0
    | Purple = 1
    | Green = 2
    | Red = 3

type Piece =
    | Turtle of Color * Direction
    | StoneWall
    | IceWall
    | Box
    | Gem of Color

type Card =
    | TurnLeft
    | TurnRight
    | MoveForward
    | FireLaser
    // TODO: | FunctionFrog
    // Note: I also omitted the Bug card... only need to add it if using this in a game UI.

type MoveResult =
    | Ok
    | Bug
    | Win

//
// Helper type for encoding transformations to game state
//
type CellUpdate = { X: int; Y: int; Piece: Option<Piece> }

//
// Board represents current game state
//    
type Board(cells: Option<Piece>[,], cards: List<Card> array, playedCards: List<Card> array) = class
    //
    // Helper method to make sure an input is valid
    // i.e. can't play a card that isn't in your deck
    // Note: this goes counter to the official rules that say you can pull cards
    //       from the beginning of your program and reuse them.
    //       I think the challenge is more interesting with limited resources.
    //
    let validateCard color card =
        if not (List.exists(fun c -> c = card) cards.[(int)color]) then
            invalidArg "card" (sprintf "%A Turtle has no %A cards remaining" color card)

    //
    // Helper method to clone the current cells and apply a sequence of updates
    //
    let cellsWith cellUpdates =
        let newCells = Array2D.copy cells
        cellUpdates
            |> Seq.iter
                (fun update ->
                    let { X = x; Y = y; Piece = piece} = update
                    newCells.[x, y] <- piece
                )
        newCells

    //
    // Helper method to generate a copy of the player decks (minus one that was just played)
    // Assumes that the card exists in the list
    //
    let remove color card =
        let newCards = Array.copy cards
        let index = List.findIndex (fun c -> c = card) newCards.[(int)color]
        newCards.[(int)color] <-
            newCards.[(int)color]
                |> List.mapi (fun i c -> if i = index then None else Some c)
                |> List.choose (fun c -> c)
        newCards

        
    //
    // Helper method to find the position of a turtle, given a turtle colour
    //
    member public this.GetTurtle turtleColor =
        let rec nextCell x y =
            // If we hit the end of the column then move to the first column of the next row
            if (x = this.X) then
                nextCell 0 (y + 1)

            // If we moved past the last row then the turtle isn't on the board
            else if (y = this.Y) then
                None

            // We're at a legitimate cell... what is in the cell?
            else
                match (cells.[x, y]) with
                | (Some ((Turtle (color, _)) as turtle)) when color = turtleColor ->
                    Some (x, y, turtle) // Found it! Note, return the turtle too so we can get directional information
            
                | _ ->
                    nextCell (x + 1) y // Try the next cell in this column
        
        // Start searching in the first column of the first row
        nextCell 0 0


    //
    // Given a turtle colour and an input card, return the next game state
    //
    member public this.Move (turtleColor: Color) (card: Card) =
        validateCard turtleColor card

        // Generate the next state of cards where the current card is no longer available
        let newCards = remove turtleColor card

        // Append the current card to the list of played cards
        let newPlayedCards =
            let newArray = Array.copy playedCards
            newArray.[(int)turtleColor] <- newArray.[(int)turtleColor] @ (card :: [])
            newArray

        // Find out where the specified turtle is currently sitting on the game board
        let (x, y, turtle) =
            match this.GetTurtle turtleColor with
            | Some result -> result
            | None ->
                printfn "%A" cells // Dump the current state for debugging
                invalidArg "turtleColor" (sprintf "%A Turtle is not on the game board" turtleColor)

        let facing =
            match turtle with
            | Turtle(_, direction) -> direction
            | _ -> invalidArg "turtle" (sprintf "Found %A instead of turtle" turtle) // Will never hit this. Make the compiler happy

        // Determine if a box can be pushed
        let canPush toX toY =
            // Calculate coordinates for where the box will be pushed to
            let pushX = toX + toX - x
            let pushY = toY + toY - y
            
            if pushX < 0 || pushX >= this.X || pushY < 0 || pushY >= this.Y then
                false // Can't push a box off the edge of the board
            else
                cells.[pushX, pushY].IsNone // Can only push a box into an empty cell
            
        // Generate new state given a new turtle direction (e.g. turning left or right)
        let newDirection dir = 
            (Ok,
                new Board(
                    cellsWith ({ X = x; Y = y; Piece = Some(Turtle (turtleColor, dir)) } :: []),
                    newCards,
                    newPlayedCards
                )
            )

        // Generate new state given a walking direction
        let walkTo toX toY =
            let (moveResult, cellUpdates) =
                // Can't walk off the edge of the board
                if toX < 0 || toX >= this.X || toY < 0 || toY >= this.Y then
                    (Bug, [])
                else
                    match cells.[toX, toY] with
                    | Some Box when canPush toX toY -> // Push the box unless it cannot be pushed
                        (Ok,
                            { X = x; Y = y; Piece = None } ::
                            { X = toX; Y = toY; Piece = Some(turtle) } ::
                            { X = toX + toX - x; Y = toY + toY - y; Piece = Some(Box) } ::
                            []
                        )

                    // Note: official game rules say that any colour gem can be picked up
                    | Some (Gem(gemColor)) when gemColor = turtleColor ->
                        (Win,
                            { X = x; Y = y; Piece = None } ::
                            { X = toX; Y = toY; Piece = Some(turtle) } ::
                            []
                        )

                    | None -> // May walk into an emtpy cell
                        (Ok,
                            { X = x; Y = y; Piece = None } ::
                            { X = toX; Y = toY; Piece = Some(turtle) } ::
                            []
                        )

                    | _ -> // Nope, can't walk in this direction...
                        (Bug, [])
            
            (moveResult, new Board(cellsWith cellUpdates, newCards, newPlayedCards))


        // Generate new state given the firing of a laser
        let fireLaser xSeq ySeq =
            let rec lookForIceWall xSeq ySeq =
                match (xSeq, ySeq) with
                // Firing along the X axis
                | (head :: tail, []) ->
                    match cells.[head, y] with
                    | Some IceWall ->
                        { X = head; Y = y; Piece = None } :: [] // Found an IceWall! Melt it.
                    | None ->
                        lookForIceWall tail [] // Found an empty square, continue firing along path
                    | _ ->
                        [] // Found non-empty, non-IceWall cell. Terminate laser.

                // Firing along the Y axis
                | ([], head :: tail) ->
                    match cells.[x, head] with
                    | Some IceWall ->
                        { X = x; Y = head; Piece = None } :: [] // Found an IceWall! Melt it.
                    | None ->
                        lookForIceWall [] tail // Found an empty square, continue firing along path
                    | _ ->
                        [] // Found non-empty, non-IceWall cell. Terminate laser.

                | _ -> // Found no ice walls... so return an empty change set
                    []

            (Ok, new Board(cellsWith (lookForIceWall xSeq ySeq), newCards, newPlayedCards))


        // Main body of the Move method. Make a move based on the input card and direction of the turtle
        match card with
        | TurnLeft ->
            match facing with
            | North -> newDirection West
            | East -> newDirection North
            | South -> newDirection East
            | West -> newDirection South

        | TurnRight ->
            match facing with
            | North -> newDirection East
            | East -> newDirection South
            | South -> newDirection West
            | West -> newDirection North

        | MoveForward ->
            match facing with
            | North -> walkTo x (y - 1)
            | East -> walkTo (x + 1) y
            | South -> walkTo x (y + 1)
            | West -> walkTo (x - 1) y
            
        | FireLaser ->
            match facing with
            | North -> fireLaser [] [y - 1 .. -1 .. 0] 
            | East -> fireLaser [x + 1 .. this.X - 1] []
            | South -> fireLaser [] [y + 1 .. this.Y - 1]
            | West -> fireLaser [x - 1 .. -1 .. 0 ] []

        // TODO: | FunctionFrog ->
        // TODO: | FixBug -> // If this gets used in a game UI, will want to expose the Bug card


    //
    // Helper method to clone the current board and apply specified updates
    // (Used in creating a new board with non-empty cell contents)
    //
    member this.withUpdates updates =
        new Board(cellsWith updates, cards, playedCards)

    //
    // Initialize an empty board and starting player decks
    //
    new() =
        let emptyBoard = Array2D.create 8 8 None

        let makeCards card n = List.init n (fun _ -> card)
        let startingCards =
            (makeCards MoveForward 18) @
            (makeCards TurnLeft 8) @
            (makeCards TurnRight 8) @
            (makeCards FireLaser 5) (* @
            (makeCards FunctionFrog 5) *)

        let allPlayersStartingCards = Array.create 4 startingCards
        
        let noCardsPlayedYet = Array.create 4 []

        new Board(emptyBoard, allPlayersStartingCards, noCardsPlayedYet)

    //
    // Initialize a board using the specified cell contents
    //
    static member Initialize(startingCells: CellUpdate list) =
        let emptyBoard = new Board()
        emptyBoard.withUpdates startingCells

    //
    // Helper method to report what card types remain for a given colour
    //
    member public this.GetPossibleCards (color: Color) =
        cards.[(int)color]
            |> Set.ofList // Convert to set to eliminate duplicates
            |> List.ofSeq // Convert back to a list

    //
    // Helper method to return the list of cards played by a player so far
    //
    member public this.GetPlayedCards (color: Color) =
        playedCards.[(int)color]

    //
    // Method to find the position of a gem, given a gem colour
    //
    member public this.LocateGem gemColor =
        let rec nextCell x y =
            // If we hit the end of the column then move to the first column of the next row
            if (x = this.X) then
                nextCell 0 (y + 1)

            // If we moved past the last row then the turtle isn't on the board
            else if (y = this.Y) then
                None

            // We're at a legitimate cell... what is in the cell?
            else
                match (cells.[x, y]) with
                | (Some (Gem color)) when color = gemColor ->
                    Some (x, y) // Found it!
            
                | _ ->
                    nextCell (x + 1) y // Try the next cell in this column
        
        // Start searching in the first column of the first row
        nextCell 0 0

    member this.X = Array2D.length1 cells
    member this.Y = Array2D.length2 cells
end