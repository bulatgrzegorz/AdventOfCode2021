open System
open System.IO
open System.Diagnostics

let move1 (x,y) instruction n = 
    match instruction with
    | "forward" -> (x + n, y)
    | "up"      -> (x, y - n)
    | "down"    -> (x, y + n)
    | _ -> raise (ArgumentException("Not supported instruction"))

let move2 (x,y,aim) instruction n = 
    match instruction with
    | "forward" -> (x + n, y + aim * n, aim)
    | "up"      -> (x, y, aim - n)
    | "down"    -> (x, y, aim + n)
    | _ -> raise (ArgumentException("Not supported instruction"))

let mulT (x, y) = x*y
let mulT3 (x, y, _) = x*y

let getInputFile = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")

let instructions = File.ReadLines(getInputFile) |> 
                    List.ofSeq |>
                    List.map (fun x -> x.Split [|' '|]) |>
                    List.map (fun x -> (x[0], int x[1]))

instructions |> List.fold (fun pos (i, n) -> move1 pos i n) (0, 0) |> mulT |> printf "%d"

instructions |> List.fold (fun pos (i, n) -> move2 pos i n) (0, 0, 0) |> mulT3 |> printf "%d"
