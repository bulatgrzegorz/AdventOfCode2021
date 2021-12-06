open System
open System.IO
open System.Diagnostics

let inline isEmpty s = match s with | null -> true | "" -> true | _ -> false

let parseDrawn (s: string) = s.Split [|','|] |> Seq.toList |> List.map int
let parseLine (s: string) = s.TrimStart([|' '|]).Replace("  ", " ").Split [|' '|] |> Seq.toList |> List.map int

let checkBoards number boards =
    boards |> List.map(fun x -> List.map(fun elem -> 
                                    match elem with
                                    | (n, 0) when n = number -> (number, 1)
                                    | (n, p) -> (n, p)) x)

let wonIndexes = List.append [ for i in 0 .. 4 -> [i*5..i*5 + 4] ] [ for i in 0 .. 4 -> [i..5..20+i] ]
let contains (xs: int list) (ys: int list) = xs |> List.fold(fun acc x -> acc && List.contains x ys) true
let isBoardWon board = wonIndexes |> List.exists(fun x -> contains x (board |> List.indexed |> List.filter(fun (i, (v, c)) -> c = 1) |> List.map(fun (i, _) -> i)))
let getWonBoard boards = boards |> List.tryFind(fun x -> isBoardWon x)
let getWonBoards boards = boards |> List.indexed |> List.filter(fun (i, x) -> isBoardWon x)
let filterOutBoard i boards = boards |> List.indexed |> List.filter(fun (ind, x) -> ind <> i) |> List.map(fun (ind, x) -> x)

let bingoGame (drawnNumbers: list<int>) boards =
    let rec bingoGameInternal currentIndex (drawnNumbers: list<int>) boards =
        match boards |> getWonBoard with
        | Some(board) -> (board, drawnNumbers[currentIndex - 1])
        | _ -> boards |> checkBoards drawnNumbers[currentIndex] |> bingoGameInternal (currentIndex+1) drawnNumbers 
    bingoGameInternal 0 drawnNumbers boards

let bingoLastGame (drawnNumbers: list<int>) boards =
    let rec bingoGameInternal currentIndex (drawnNumbers: list<int>) boards =
        match boards |> getWonBoards with
        | [(i, board)] when boards.Length = 1 -> (board, drawnNumbers[currentIndex - 1])
        | [(i, _)] -> boards |> filterOutBoard i |> checkBoards drawnNumbers[currentIndex] |> bingoGameInternal (currentIndex+1) drawnNumbers
        | (i, _)::rest -> boards |> filterOutBoard i |> bingoGameInternal currentIndex drawnNumbers  
        | [] -> boards |> checkBoards drawnNumbers[currentIndex] |> bingoGameInternal (currentIndex+1) drawnNumbers
    bingoGameInternal 0 drawnNumbers boards

let calculateResult wonBoard wonNumber = 
    let unmarkedSum = wonBoard |> List.filter(fun (n,c) -> c = 0) |> List.map(fun (n,c) -> n) |> List.fold (+) 0
    unmarkedSum * wonNumber

let doBingoGame drawnNumbers bingoBoards = 
    let (wonBoard, wonNumber) = bingoGame drawnNumbers bingoBoards
    calculateResult wonBoard wonNumber

let doLastBingoGame drawnNumbers bingoBoards = 
    let (wonBoard, wonNumber) = bingoLastGame drawnNumbers bingoBoards
    calculateResult wonBoard wonNumber


let getInputFile = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")
let inputLines = File.ReadLines(getInputFile) |> List.ofSeq

let drawnNumbers = inputLines |> List.head |> parseDrawn
let bingoBoards = inputLines 
                |> List.skip 2 
                |> List.filter(fun x -> (not << isEmpty) x) 
                |> List.chunkBySize 5 
                |> List.map(fun x -> List.map(fun y -> parseLine y) x |> List.concat |> List.map(fun y -> (y, 0)))

printfn "Part 1: %d\n" (doBingoGame drawnNumbers bingoBoards)

printfn "Part 2: %d\n" (doLastBingoGame drawnNumbers bingoBoards)
