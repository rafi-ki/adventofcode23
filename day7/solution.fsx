open System
open System.IO

type Card = char

type HandType = Five = 0 | Four = 1 | FullHouse = 2 | Three = 3 | TwoPair = 4 | OnePair = 5 | Highest = 6

type Hand = { Card: string }

type BidHand = { Hand: string; Bid: int; Type: HandType; Rank: int option }

let input =
    let text = File.ReadAllText "example.txt"
    text.Split Environment.NewLine

let parseType (hand: string) =
    let grouped = hand |> Seq.groupBy id |> Seq.map (fun (_, l) -> l |> Seq.length)
    let max = Seq.max grouped
    let length = Seq.length grouped
    match max, length with
    | 5, _ -> HandType.Five
    | 4, _ -> HandType.Four
    | 3, 2 -> HandType.FullHouse
    | 3, _ -> HandType.Three
    | 2, 3 -> HandType.TwoPair
    | 2, _ -> HandType.OnePair
    | _, _ -> HandType.Highest

let parse (line: string) =
    let split = line.Split ' '
    let handString = split[0]
    let handType = parseType handString
    let bid = split[1] |> int
    let h = { Hand = handString; Bid = bid; Rank = None; Type = handType }
    printfn $"%A{h}"
    h

let solution1 =
    input
    |> Array.map parse
    |> Array.length

let solution2 =
    2

let run = printf $"solution: %i{solution1}"