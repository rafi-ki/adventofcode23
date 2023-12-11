open System
open System.IO

let input =
    let text = File.ReadAllText "input.txt"
    text.Split Environment.NewLine

let toNumbers (line: string) = line.Split ' ' |> Array.map int

let calcDistance (numbers: int array) = numbers |> Array.pairwise |> Array.map (fun (a, b) -> b-a)

let rec calculateDiff (numbers: int array) nextValueFun =
    if numbers |> Array.forall (fun x -> x = 0) then
        0
    else
        let distanceOfNextRow = calcDistance numbers
        let diff = calculateDiff distanceOfNextRow nextValueFun
        nextValueFun (numbers, diff)

let valueFor nextValueFun (line: string) = calculateDiff (toNumbers line) nextValueFun

let solution1 =
    let (firstHalf: int64) =
        input
        |> Array.take 100
        |> Array.map (valueFor (fun (x, diff) -> (Array.last x) + diff))
        |> Array.sum
        |> int64
    let (secondHalf: int64) =
        input
        |> Array.skip 100
        |> Array.map (valueFor (fun (x, diff) -> (Array.last x) + diff))
        |> Array.sum
        |> int64
    firstHalf + secondHalf

let solution2 =
    input
    |> Array.map (valueFor (fun (x, diff) -> (x |> Array.head) - diff))
    |> Array.sum

let run = printf $"solution: %i{solution2}"