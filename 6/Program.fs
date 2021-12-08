open System
open System.IO
open System.Diagnostics

let getInputFile = Path.Combine(System.IO.Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "input.txt")
let parseNumbers (s: string) = s.Split [|','|] |> Seq.toList |> List.map int
let fishes = File.ReadLines(getInputFile) |> List.ofSeq |> List.head |> parseNumbers


// let fa = List.toArray fishes
// let countNumbers stopAt fishes =
//     let rec t toAdd day stopAt fishes =
//         Console.WriteLine("Day: " + string day)
//         Console.WriteLine(fishes |> List.map (sprintf "%A") |> String.concat ",") 
//         match fishes |> List.indexed |> List.tryFind(fun (_, x) -> x = 0) with
//         | Some(i, _) -> fishes |> List.updateAt i 6 |> t (toAdd @ [9]) day stopAt 
//         | _ -> if day = stopAt then fishes |> List.length else t [] (day+1) stopAt ((fishes @ toAdd) |> List.map(fun x -> x - 1)) 
//     t [] 0 5 fishes

let rec f c n = 
    match (c, n) with
    | (c, n) when n >= 7 -> f c (n-7) + f ((c+2)%9) (n-7)
    | _ -> 1
    // | (c, n) when c <= n-1 -> 2
    // | _  -> 1


// let rec f n = 2 * f (n-7)

// let rec f8 n = 123// f8 (n-7) + f1 (n-7)
// let rec f0 n = 1234//f0 (n-7) + f2 (n-7)
// let rec f7 n = f7 (n-7) + f0 (n-7)
// let rec f6 n = f6 (n-7) + f8 (n-7)
// let rec f5 n = f5 (n-7) + f7 (n-7)
// let rec f4 n = f4 (n-7) + f6 (n-7)
// let rec f3 n = f3 (n-7) + f5 (n-7)
// let rec f2 n = f2 (n-7) + f4 (n-7)
// let rec f1 n = f1 (n-7) + f3 (n-7)
// let rec countNumbersA stopAt days fa =
//     let rec countNumbersAI stopAt fa = function
//                                         | x when x = stopAt -> fa |> Array.length
//                                         | _ -> countNumbersA stopAt (days + 1) (fa |> Array.map(fun x -> if x = 0 then [|6;8|] else [|x-1|]) |> Array.concat )
//     countNumbersAI stopAt fa days
let stopAt = 256
let rec countNumbers stopAt days fishes =
    let rec accumulateInternal (accumulator: int list) = function
      | [] -> accumulator
      | x :: xs -> accumulateInternal (if x = 0 then 9::7::accumulator else x::accumulator) xs
    if days = stopAt then fishes |> List.length
    else countNumbers stopAt (days+1) (accumulateInternal [] fishes |> List.map(fun x -> x - 1))

// let a = fishes |> List.countBy(fun x -> x) |> List.map(fun (num, c) -> c * countNumbers 0 [num]) |> List.fold (+) 0
    // match days with
    // | x when x = stopAt -> fishes |> List.length
    // | _ -> countNumbers stopAt (days + 1) (fishes |> List.map(fun x -> if x = 0 then [6;8] else [x-1]) |> List.concat )
// printfn "%d\n" (countNumbers 0 [1])
printfn "%d\n" (countNumbers stopAt 0 fishes)