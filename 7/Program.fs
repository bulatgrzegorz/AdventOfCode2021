open System
open System.IO
open System.Diagnostics

let getInputFile = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")
let parseNumbers (s: string) = s.Split [|','|] |> Seq.toList |> List.map int
let crabs = File.ReadLines(getInputFile) |> List.ofSeq |> List.head |> parseNumbers

let lower x y = if x <= y then x else y
let bigger x y = if x <= y then y else x

let possibleDestinations c = [(c |> List.min)..(c |> List.max)] |> List.distinct

let simpleCost d = crabs |> List.fold(fun acc x -> acc + abs (d-x)) 0
let growingCost d = crabs |> List.fold(fun acc x -> acc + ([(lower x d)..(bigger x d)] |> List.indexed |> List.map(fun x -> fst x) |> List.sum)) 0

crabs |> possibleDestinations |> List.map(fun x -> simpleCost x) |> List.min |> printfn "Part 1: %d\n"
crabs |> possibleDestinations |> List.map(fun x -> growingCost x) |> List.min |> printfn "Part 2: %d\n"