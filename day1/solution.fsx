open System
open System.IO

let input =
    let text = File.ReadAllText "example.txt"
    text.Split Environment.NewLine

let isDigit (x:Char) = Char.IsDigit x

let numberChars = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; ]

let mapNumbers number = match number with
                        | "one" -> "1"
                        | "two" -> "2"
                        | "three" -> "3"
                        | "four" -> "4"
                        | "five" -> "5"
                        | "six" -> "6"
                        | "seven" -> "7"
                        | "eight" -> "8"
                        | "nine" -> "9"
                        | x -> x

// let digits (line: string) =
//     let first = line |> Seq.find isDigit
//     let last = line |> Seq.findBack isDigit
//     let digits = first.ToString() + last.ToString()
//     digits |> int

let matches (line: string) (number: string) = line.Contains number

let matchesFor (line: string) (numbers: string list) =
    let matchingNumbers = numbers |> List.filter (matches line)
    let indexesOfMatchingNumbers = matchingNumbers |> List.map (fun x -> (line.IndexOf x, x))
    let result = indexesOfMatchingNumbers |> List.sortBy fst
    let firstDigit = result |> List.head |> snd |> mapNumbers
    let lastDigit = result |> List.last |> snd |> mapNumbers
    Console.WriteLine(firstDigit + lastDigit)
    let digits = firstDigit.ToString() +  lastDigit.ToString()
    digits |> int

let digitsFromText (line: string) = matchesFor line numberChars


// let solution1 =
//     input
//     |> Array.map digits
//     |> Array.sum

let solution2 =
    input
    |> Array.map digitsFromText
    |> Array.sum

let run = printf $"solution: %i{solution2}"