open System
open System.IO
open System.Diagnostics

[<EntryPoint>]
let main(args) = 
    let getWorkingDirectory = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")
    let p a x y = if x < y then a + 1 else a
    let ft (x, _, _) = x
    let sumElements x = List.reduce(fun ac y -> ac + y) x
    
    let countBiggerThenPrevious l = List.fold (fun (acc, pr) x -> (p acc pr x, x)) (0, Int32.MaxValue) l |> fst |> printf "%d\n"

    let parsedInput = File.ReadLines(getWorkingDirectory) |> 
                        List.ofSeq |>
                        List.map int

    countBiggerThenPrevious parsedInput     

    parsedInput |> List.windowed 3 |> List.map(fun x -> sumElements x) |> countBiggerThenPrevious
    
    0
