open System
open System.IO
open System.Diagnostics

let getInputFile = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")
let boolToInt (b:bool):int = System.Convert.ToInt32(b)

let sumElements x = x |> List.fold (+) 0

let countBiggerThenPrevious l = List.windowed 2 l |> List.fold (fun acc x -> acc + boolToInt (x[0] < x[1])) 0

let parsedInput = File.ReadLines(getInputFile) |> 
                    List.ofSeq |>
                    List.map int

parsedInput |> countBiggerThenPrevious |> printf "Part 1: %d\n"

parsedInput |> List.windowed 3 |> List.map(fun x -> sumElements x) |> countBiggerThenPrevious |> printf "Part 2: %d\n"
