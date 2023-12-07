open System
open System.IO

type Race =
    {
        Time: int;
        Distance: int;
    }


let input = File.ReadAllText "example.txt"
    // text.Split Environment.NewLine

let mapToRace tupleArray = tupleArray |> Array.map (fun (t, d) -> { Time = t |> int; Distance = d |> int })

let parse (text: string) =
    let lines = text.Split Environment.NewLine
    let timeLine = lines[0].Split ':'
    let distanceLine = lines[1].Split ':'
    let times = timeLine[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
    let distances = distanceLine[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
    mapToRace (Array.zip times distances)

let countWins (race: Race) =
    2

let solution1 =
    input
    |> parse
    |> Array.map countWins
    |> Array.fold (fun total n -> total * n) 1

// let solution2 =
//     input
//     |> Array.map parseToGame
//     |> Array.map mapToMaxBag
//     |> Array.map multiplySetOfCubes
//     |> Array.sum

let run = printf $"solution: %i{solution1}"