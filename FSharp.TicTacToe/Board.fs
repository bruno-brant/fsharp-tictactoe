module Board

type board = char array2d


type Position = int * int

// Possible moves from the player
type Move = 
    | Left
    | Right
    | Up
    | Down
    | Select

let newBoard ()  =
    Array2D.create 3 3 ' '

// Apply movement to the board
let processSelected (line, column) move =
    match move with 
    | Left -> 
        (line, max 0 (column - 1))
    | Right ->
        (line, min 2 (column + 1))
    | Up ->
        (max 0 (line - 1), column)
    | Down ->
        (min 2 (line + 1), column)
    | _ -> 
        (line, column)
    
// Prints the board
let printBoard (board : char array2d) (selected : int*int) = 
    let b line' column' = 
        if (line', column') = selected then "█ "
        else (board.[line', column'] |> string) + " "

    printf "╔═══╤═══╤═══╗\n"        
    printf "║ %s│ %s│ %s║\n" (b 0 0) (b 0 1) (b 0 2)
    printf "╟───┼───┼───╢\n"
    printf "║ %s│ %s│ %s║\n" (b 1 0) (b 1 1) (b 1 2)
    printf "╟───┼───┼───╢\n"
    printf "║ %s│ %s│ %s║\n" (b 2 0) (b 2 1) (b 2 2)
    printf "╚═══╧═══╧═══╝\n"
