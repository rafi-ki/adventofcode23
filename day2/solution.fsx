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
    let text = File.ReadAllText "input.txt"
    text.Split Environment.NewLine

let maxRed = 12
let maxGreen = 13
let maxBlue = 14

let parseSetOfCubes (line: string) =
    let cubes = line.Split ','
    let redsString = cubes |> Array.tryFind (fun x -> x.Contains "red")
    let bluesString = cubes |> Array.tryFind (fun x -> x.Contains "blue")
    let greensString = cubes |> Array.tryFind (fun x -> x.Contains "green")
    let reds = match redsString with
                | Some (x: string) -> x.Replace(" red", "") |> int
                | None -> 0
    let blues = match bluesString with
                | Some (x: string) -> x.Replace(" blue", "") |> int
                | None -> 0
    let greens = match greensString with
                    | Some (x: string) -> x.Replace(" green", "") |> int
                    | None -> 0
    { Red = reds; Blue = blues; Green = greens }

let parseToGame (line: string) =
    let gameSplit = line.Split ':'
    let gamePart = gameSplit[0].Split ' '
    let game = gamePart[1]
    let cubesParts = gameSplit[1].Split ';'
    let setOfCubes = cubesParts |> Array.map parseSetOfCubes
    { Index = game |> int; Bags = Array.toList setOfCubes }

let aboveLimits (game: Game) =
    let aboveLimitRed = game.Bags |> List.exists (fun x -> x.Red > maxRed)
    let aboveLimitGreen = game.Bags |> List.exists (fun x -> x.Green > maxGreen)
    let aboveLimitBlue = game.Bags |> List.exists (fun x -> x.Blue > maxBlue)
    aboveLimitBlue || aboveLimitGreen || aboveLimitRed

let solution1 =
    input
    |> Array.map parseToGame
    |> Array.filter (fun x -> not (aboveLimits x))
    |> Array.sumBy (fun x -> x.Index)

// let solution2 =
//     input
//     |> Array.map digitsFromText
//     |> Array.sum

let run = printf $"solution: %i{solution1}"