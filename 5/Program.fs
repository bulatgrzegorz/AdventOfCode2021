open System
open System.IO
open System.Diagnostics

let getInputFile = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")
let inputLines = File.ReadLines(getInputFile) |> List.ofSeq


let parsePosition (s: string) = 
    let cor = s.Split([|','|])
    (int cor[0], int cor[1]) 
let parseLine (s: string) = 
    let pos = s.Split([|" -> "|], StringSplitOptions.None)
    (parsePosition pos[0], parsePosition pos[1]) 

let compareToInt x y = if x < y then x + 1 else if x = y then x else x - 1

let rec allPointsPart1 (pos1, pos2) =
    match (pos1, pos2) with
    | ((x, y), (k, l)) when x = k && y = l -> [(x, y)]
    | ((x, y), (k, l)) when x = k || y = l -> (x,y) :: allPointsPart1 ((compareToInt x k, compareToInt y l), (k, l))
    | _ -> []

let rec allPointsPart2 (pos1, pos2) =
    match (pos1, pos2) with
    | ((x, y), (k, l)) when x = k && y = l -> [(x, y)]
    | ((x, y), (k, l)) -> (x,y) :: allPointsPart2 ((compareToInt x k, compareToInt y l), (k, l))

let countPoints f = File.ReadLines(getInputFile)
                    |> List.ofSeq 
                    |> List.map(fun x -> parseLine x)
                    |> List.map(fun x -> f x)
                    |> List.concat
                    |> List.countBy(fun x -> x)
                    |> List.filter(fun ((_), c) -> c > 1) |> List.length

printfn "Part 1: %d\n" (countPoints allPointsPart1)

printfn "Part 2: %d\n" (countPoints allPointsPart2)
