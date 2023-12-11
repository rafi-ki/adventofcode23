open System
open System.IO

let input =
    let text = File.ReadAllText "input.txt"
    text.Replace('7', 'T')

type Direction = NORTH | SOUTH | EAST | WEST

let possibleMoves c =
    match c with
    | 'S' -> [NORTH; SOUTH; EAST; WEST]
    | '|' -> [NORTH; SOUTH]
    | '-' -> [EAST; WEST]
    | 'L' -> [NORTH; EAST]
    | 'J' -> [NORTH; WEST]
    | 'T' -> [SOUTH; WEST]
    | 'F' -> [SOUTH; EAST]
    | _ -> []

let nextPoint x y direction =
    let result = match direction with
                    | EAST -> (x, y+1)
                    | WEST -> (x, y-1)
                    | NORTH -> (x-1, y)
                    | SOUTH -> (x+1, y)
    result

let traversablePoint area point =
    let length = area |> Array2D.length2
    let isDigitOrPoint c = (Char.IsDigit c) || c = '.'
    match point with
    | x, y when x >= length || y >= length || x < 0 || y < 0 -> false
    | x, y when isDigitOrPoint area[x, y] -> false
    | _, _ -> true

let rec traverse (area: char[,]) (point: int*int) =
    let x, y = point
    let mutable points = [(x, y, 0)]
    let mutable currentDistance = 0
    while points.IsEmpty |> not do
        let x, y, d = points.Head
        let distanceChar = d.ToString()[0]
        let newPoints = possibleMoves area[x, y]
                                 |> List.map (nextPoint x y)
                                 |> List.filter (traversablePoint area)
                                 |> List.map (fun (i, t) -> (i, t, d+1))
        area[x, y] <- distanceChar
        points <- ((List.tail points) @ newPoints)
        currentDistance <- d
        if (d > currentDistance) then currentDistance <- d
    printfn $"%A{area}"
    currentDistance


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
    traverse area (startX, startY)

// let solution2 =
//     input
//     |> Array.map (valueFor (fun (x, diff) -> (x |> Array.head) - diff))
//     |> Array.sum

let run = printf $"solution: %i{solution1}"