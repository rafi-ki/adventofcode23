open System
open System.Collections.Generic
open System.IO

type Card =
    {
        Numbers: int array;
        WinningNumbers: int array;
    }

type InstanceCounter =
    {
        mutable Copies: int array;
        Index: int
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

// let calculate (number: int) = let number1 = number - 1
//                               match number with
//                               | 0 -> 0
//                               | 1 -> 1
//                               | _ -> Math.Pow(2, number1) |> int

// let solution1 =
//     input
//     |> Array.map parse
//     |> Array.map countWinningNumbers
//     |> Array.map calculate
//     |> Array.sum

let calculate (acc: InstanceCounter) (ele: Card) =
    let matches = countWinningNumbers ele
    printfn $"index: {{%i{acc.Index}}}, matches: {{%i{matches}"
    printfn $"copies: {{%A{acc.Copies}}}"
    let copiesOfLastIndex = if acc.Index = 0 then 1 else acc.Copies[acc.Index]
    printfn $"current copies: {{%A{copiesOfLastIndex}}}"
    for i in acc.Index + 1 .. acc.Index + matches do
       acc.Copies[i] <- (acc.Copies[i] + copiesOfLastIndex)
    { acc with Index = acc.Index + 1 }

let solution2 =
    let cards = input |> Array.map parse
    let copyInit = [| for _ in 0 .. cards.Length - 1 -> 1 |]
    let result = cards |> Array.fold calculate { Copies = copyInit; Index = 0 }
    result.Copies |> Array.sum


let run = printf $"solution: %i{solution2}"