open System
open System.IO

type HandType = Five = 0 | Four = 1 | FullHouse = 2 | Three = 3 | TwoPair = 4 | OnePair = 5 | Highest = 6

type BidHand = { Hand: string; Bid: int; Type: HandType; Rank: int }

let inline charToInt c = int c - int '0'

let mapCard (c: char) =
    if Char.IsDigit c then charToInt c
    else
    match c with
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "impossible"

let input =
    let text = File.ReadAllText "input.txt"
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
    { Hand = handString; Bid = bid; Rank = -1; Type = handType }

let sort (bidHand1: BidHand) (bidHand2: BidHand) =
    if bidHand1 = bidHand2 then 0
    else if bidHand1.Type <> bidHand2.Type then bidHand1.Type.CompareTo bidHand2.Type
    else
        let result = Seq.zip bidHand1.Hand bidHand2.Hand
                            |> Seq.skipWhile (fun (a, b) -> a = b)
                            |> Seq.head
        mapCard (snd result) - mapCard (fst result)

let rank (bidHands: BidHand array) =
    bidHands
    |> Array.sortWith sort
    |> Array.rev
    |> Array.mapi (fun i hand -> { hand with Rank = i + 1 })

let solution1 =
    let ranked = input |> Array.map parse |> rank
    ranked
    |> Array.map (fun x -> x.Bid * x.Rank)
    |> Array.sum

let solution2 =
    2

let run = printf $"solution: %i{solution1}"