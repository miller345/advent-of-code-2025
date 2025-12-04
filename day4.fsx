#load "utils.fsx"
open Utils

let parsed =
    let parseLine (str: string) = str |> Seq.toArray |> Array.map string
    let parsed = System.IO.File.ReadAllLines "./inputs/4.txt" |> Array.map parseLine
    let width = parsed.[0].Length
    let height = parsed.Length
    let grid = Array2D.init height width (fun y x -> parsed.[y].[x])
    grid

let getNeighbours (y, x) grid =
    [ -1, 0; -1, 1; 0, 1; 1, 1; 1, 0; 1, -1; 0, -1; -1, -1 ] // neighbour offsets
    |> List.map (fun (dy, dx) -> y + dy, x + dx) // neigbour coords
    |> List.filter (fun (y, x) -> y >= 0 && y < Array2D.length1 grid && x >= 0 && x < Array2D.length2 grid) // remove invalid
    |> List.map (fun (y, x) -> grid.[y, x]) // get values
    |> List.toArray


let part1 grid =
    let mutable count = 0

    for y = 0 to Array2D.length1 grid - 1 do
        for x = 0 to Array2D.length2 grid - 1 do
            let rollCount = getNeighbours (y, x) grid |> Array.filter ((=) "@") |> Array.length
            count <- count + (if rollCount < 4 && grid.[y, x] = "@" then 1 else 0)

    count

part1 parsed |> log
