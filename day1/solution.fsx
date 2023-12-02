open System
open System.IO

let input =
    let text = File.ReadAllText "example.txt"
    text.Split Environment.NewLine

let isDigit (x:Char) = Char.IsDigit x

let numberChars = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]


let digits (line: string) =
    let first = line |> Seq.find isDigit
    let last = line |> Seq.findBack isDigit
    let digits = first.ToString() + last.ToString()
    digits |> int

let digitsFromText (line: string) =
    let first = line |> Seq.find isDigit
    let last = line |> Seq.findBack isDigit
    let digits = first.ToString() + last.ToString()
    digits |> int

let solution1 =
    input
    |> Array.map digits
    |> Array.sum

let solution2 =
    input
    |> Array.map digitsFromText
    |> Array.sum

let run = printf $"solution: %i{solution2}"