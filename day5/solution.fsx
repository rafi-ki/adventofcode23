open System
open System.IO

type Card =
    {
        Numbers: int array;
        WinningNumbers: int array;
    }


let input =
    let text = File.ReadAllText "input.txt"
    text.Split Environment.NewLine

let parse (line: string) =
    let cardSplit = line.Split ':'
    let numbersSplit = cardSplit[1].Split '|'
    let winningNumbers = numbersSplit[0].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim() |> int)
    let numbers = numbersSplit[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim() |> int)
    { Numbers = numbers; WinningNumbers = winningNumbers }

let countWinningNumbers (card: Card) =
    let except = Array.except card.Numbers card.WinningNumbers
    card.WinningNumbers.Length - except.Length

let calculate (number: int) = let number1 = number - 1
                              match number with
                              | 0 -> 0
                              | 1 -> 1
                              | _ -> Math.Pow(2, number1) |> int

let solution1 =
    input
    |> Array.map parse
    |> Array.map countWinningNumbers
    |> Array.map calculate
    |> Array.sum

// let solution2 =
//     input
//     |> Array.map parseToGame
//     |> Array.map mapToMaxBag
//     |> Array.map multiplySetOfCubes
//     |> Array.sum

let run = printf $"solution: %i{solution1}"