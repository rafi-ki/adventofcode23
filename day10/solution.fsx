open System
open System.IO

let input =
    let text = File.ReadAllText "example.txt"
    text.Replace('7', 'T')

type Direction = NORTH | SOUTH | EAST | WEST


let possibleMoves c =
    match c with
    | 'S' -> [|NORTH; SOUTH; EAST; WEST|]
    | '.' -> [||]
    | '|' -> [|NORTH; SOUTH|]
    | '-' -> [|EAST; WEST|]
    | 'L' -> [|NORTH; EAST|]
    | 'J' -> [|NORTH; WEST|]
    | 'T' -> [|SOUTH; WEST|]
    | 'F' -> [|SOUTH; EAST|]
    | _ -> failwith "no possible!"

let nextPoint point direction =
    let x, y = point
    match direction with
    | EAST -> (x, y+1)
    | WEST -> (x, y-1)
    | NORTH -> (x-1, y)
    | SOUTH -> (x+1, y)

let traversablePoint area point =
    let xLength = area |> Array2D.length1
    let yLength = area |> Array2D.length2
    let isDigitOrPoint c = Char.IsDigit c || c = '.'
    match point with
    | x, y when x > xLength && y > yLength -> false
    | x, y when isDigitOrPoint area[x, y] -> false
    | _, _ -> true

let traverseSingle (area: char[,]) (distance: int) (point: int*int) =
    let x, y = point
    let distanceChar = distance.ToString()[0]
    let nextPoints = possibleMoves area[x, y]
                     |> Array.map (nextPoint point)
                     |> Array.filter (traversablePoint area)
    if nextPoints |> Array.isEmpty then
        area[x, y] <- distanceChar
        distance
    else
        area[x, y] <- distanceChar
        distance+1

let rec traverse (area: char[,]) (distance: int) (point: int*int) =
    let x, y = point
    let distanceChar = distance.ToString()[0]
    let nextPoints = possibleMoves area[x, y]
                     |> Array.map (nextPoint point)
                     |> Array.filter (traversablePoint area)
    // let mutable distanceMut = distance
    // for p in nextPoints do
    //     let dPlusOne = (distanceMut + 1).ToString()[0]
    //     area[fst p, snd p] <- dPlusOne
    // distanceMut <- distanceMut + 1
    if nextPoints |> Array.isEmpty then
        area[x, y] <- distanceChar
        distance
    else
        area[x, y] <- distanceChar
        // printfn $"%A{area}"
        // printfn $"nextPoints: %A{nextPoints}"
        let traverseNext = traverse area (distance+1)
        let distances = nextPoints |> Array.map traverseNext
        Array.max distances

let mapTo2dArrayWithStart (text: string) =
    let lines = text.Split Environment.NewLine |> Seq.toArray
    let lengthOfLine = lines |> Array.head |> Seq.length
    let mutable (tdArray: char[,]) = Array2D.zeroCreate lines.Length lengthOfLine
    let mutable start = (0, 0)
    for x in 0..lines.Length-1 do
        for y in 0..lengthOfLine-1 do
            tdArray[x,y] <- lines[x][y]
            if lines[x][y] = 'S' then
                start <- (x, y)
    (tdArray, start)

let solution1 =
    let area, (startX, startY) = mapTo2dArrayWithStart input
    printfn $"start is at {startX},{startY} = %A{area[startX, startY]}"
    traverse area 0 (startX, startY)

// let solution2 =
//     input
//     |> Array.map (valueFor (fun (x, diff) -> (x |> Array.head) - diff))
//     |> Array.sum

let run = printf $"solution: %i{solution1}"