open System;
open Board;
open Player;

// For more information see https://aka.ms/fsharp-console-apps
let scanf () = 
    Console.ReadLine();

/// Get the players that will play the game
let getPlayers () : (Result<player * player, string>)=
    printf "How many human players (1/2)? "
    
    // TODO: let player's choose their sign
    match (scanf () |> int) with
    | 1 -> Ok (humanPlayer, computerPlayer)
    | 2 -> Ok (humanPlayer, humanPlayer)
    | _ -> Error "Unexpected number of clients!"

/// Get the other sign
let otherSign sign =
    match sign with
    | 'x' -> 'o'
    | 'o' -> 'x'
    | _ -> Exception "Unexpected sign!" |> raise 

let renderFrame (cursorPosition : Position) board sign = 
    Console.Clear()
    Console.SetCursorPosition(0, 0)

    printBoard board cursorPosition
    printfn "" 
    printfn "It's %c turn!" sign

let processInput (cursorPosition : Position) board player = 
    let move = player board
    let nextPosition = processSelected cursorPosition move 

    (nextPosition, move)

let checkWinner (board: board) =
    let checkLine line =
        let first = board.[line, 0]
        let second = board.[line, 1]
        let third = board.[line, 2]

        first = second && second = third && first <> ' '

    let checkColumn column =
        let first = board.[0, column]
        let second = board.[1, column]
        let third = board.[2, column]

        first = second && second = third && first <> ' '

    let checkDiagonal () =
        let first = board.[0, 0]
        let second = board.[1, 1]
        let third = board.[2, 2]

        first = second && second = third && first <> ' '

    let checkAntiDiagonal () =
        let first = board.[0, 2]
        let second = board.[1, 1]
        let third = board.[2, 0]

        first = second && second = third && first <> ' '

    checkLine 0 || checkLine 1 || checkLine 2 || checkColumn 0 || checkColumn 1 || checkColumn 2 || checkDiagonal () || checkAntiDiagonal ()

/// <summary>
/// Main game loop
/// </summary>
/// <param name="board">
/// Current board
/// </param>
let rec loop  (player1: player) (player2: player) (sign: char) (board: board) (cursor: Position)  =
    renderFrame cursor board sign

    let ((line, column), move) = processInput cursor board player1

    let (nextBoard, nextSign) = 
        match move with
        | Select -> 
            board.[line, column] <- sign
            (board, otherSign sign)
        | _ -> (board, sign)

    if (checkWinner nextBoard) then
        printfn $"Player 1 won!"
    else
        loop player2 player1 nextSign nextBoard (line, column)


let startGame () = 
    printf "Let's play TicTacToe!\n"

    match getPlayers () with
    | Error e -> Error e
    | Ok (player1, player2) -> 
        loop player1 player2 'x' (newBoard ()) (0, 0) |> Ok

match startGame () with
| Ok _ -> 
    printf "Game finished!\n"
    printf "Want to play another game? (y/n)"
    match scanf () with
    | "y" -> startGame () |> ignore
    | _ -> printf "Bye!"
| Error e -> printf "Error: %s" e
