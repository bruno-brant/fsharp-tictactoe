module Player

open System
open Board

type player = board -> Move

/// Gets the next movement from the human player
let rec humanPlayer board : Move =
    Console.ReadKey true |>
        fun key -> 
            match key.Key with
            | ConsoleKey.LeftArrow -> Left
            | ConsoleKey.RightArrow -> Right
            | ConsoleKey.UpArrow -> Up
            | ConsoleKey.DownArrow -> Down
            | ConsoleKey.Enter -> Select
            | _ -> humanPlayer board

/// Computes the next move for the computer player
let computerPlayer board : Move = 
    Move.Select
