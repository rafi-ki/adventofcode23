open System
open System.IO

type SetOfCubes =
    {
        Red: int;
        Blue: int
        Green: int;
    }

type Game = { Index: int; Bags: SetOfCubes list }

let input =
    let text = File.ReadAllText "example.txt"
    text.Split Environment.NewLine

let solution1 =
    input
    |> Array.map parseToGame
    |> Array.filter (fun x -> not (aboveLimits x))
    |> Array.sumBy (fun x -> x.Index)

// let solution2 =
//     input
//     |> Array.map parseToGame
//     |> Array.map mapToMaxBag
//     |> Array.map multiplySetOfCubes
//     |> Array.sum

let run = printf $"solution: %i{solution1}"