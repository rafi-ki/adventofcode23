open System
open System.IO

type Race =
    {
        Time: int64;
        Distance: int64;
    }

let input = File.ReadAllText "input.txt"
    // text.Split Environment.NewLine

let mapToRace tupleArray = tupleArray |> Array.map (fun (t, d) -> { Time = t |> int64; Distance = d |> int64 })

let parse (text: string) =
    let lines = text.Split Environment.NewLine
    let timeLine = lines[0].Split ':'
    let distanceLine = lines[1].Split ':'
    let times = timeLine[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
    let distances = distanceLine[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
    mapToRace (Array.zip times distances)

let calcDistance (time: int) (duration: int) : int64=
    if duration = 0 then 0L
    else int64 (time - duration) * (int64 duration)
    // 0 -> 0
    // 1 -> time-1
    // 2 -> time-2 * 2
    // 3 -> time-3 * 3

let countWins (race: Race) =
    let tada = calcDistance (int race.Time)
    let possibilities = [|0.. (int race.Time)|] |> Array.map tada
    printfn $"%A{possibilities}"
    possibilities |> Array.filter (fun x -> race.Distance < x) |> Array.length

let solution1 =
    input
    |> parse
    |> Array.map countWins
    |> Array.fold (fun total n -> total * n) 1

let solution2 =
    // let race = { Time = 71530; Distance = 940200 }
    let race = { Time = 40817772; Distance = 219101213651089L }
    countWins race

let run = printf $"solution: %i{solution2}"