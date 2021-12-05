open System
open System.IO
open System.Diagnostics

let getInputFile = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")

type CommonType = Less | Most
let listOfZeroes : int list = Array.zeroCreate 12 |> List.ofArray
let inline charToInt c = int c - int '0'
let inline bitToInt i = Convert.ToInt32(i, 2)
let inline compare commonType x (y:float) equalDef = 
    match commonType with 
    | CommonType.Most -> if x = y then equalDef elif x > y then 1 else 0
    | CommonType.Less -> if x = y then equalDef elif x > y then 0 else 1
let inline binaryToInt s = s |> List.fold(fun acc x -> acc + string x) "" |> bitToInt 

let inputLines = File.ReadLines(getInputFile) |> List.ofSeq
let inputBinaries = inputLines |> List.map(fun x -> x |> Seq.toList |> List.map(fun x -> x |> charToInt)) 

let getCommons commonType d input = input |> List.fold(fun acc x -> List.mapi(fun i y -> acc[i] + y) x) listOfZeroes
                                        |> List.map(fun x -> compare commonType x (float input.Length/2.0) d)

let partOneResult = inputBinaries |> getCommons CommonType.Most 1

let multiplied = (partOneResult |> binaryToInt) * (partOneResult |> List.map(fun y -> if y = 1 then 0 else 1) |> binaryToInt)

printf "Part 1: %d\n" multiplied

let filter commonType equalDef input index =
    let mostCommonOnIndex = getCommons commonType equalDef input |> Seq.item index
    input |> List.filter(fun x -> x[index] = mostCommonOnIndex)

let rec suited commonType equalDef (input: list<list<int>>) =
    let rec suitedInternal equalDef currentIndex (input: list<list<int>>) =
        match input.Length with
        | 0 -> raise(Exception())
        | 1 -> input[0]
        | _ -> filter commonType equalDef input currentIndex |> suitedInternal equalDef (currentIndex+1)
    suitedInternal equalDef 0 input

let oxygen = suited CommonType.Most 1 inputBinaries |> binaryToInt
let c02 = suited CommonType.Less 0 inputBinaries |> binaryToInt

let r = oxygen * c02
printf "Part 2: %d\n" r 