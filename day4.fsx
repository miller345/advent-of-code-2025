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

let isRemovable (y, x) grid =
    let rollCount = getNeighbours (y, x) grid |> Array.filter ((=) "@") |> Array.length
    rollCount < 4 && grid.[y, x] = "@"

let getNextGridAndRemovedCount grid =
    let mutable count = 0

    let nextGrid =
        grid
        |> Array2D.mapi (fun y x v ->
            if isRemovable (y, x) grid then
                count <- count + 1
                "x"
            else
                v)

    nextGrid, count

let part1 grid =
    let _, count = getNextGridAndRemovedCount grid
    count

part1 parsed |> log

let part2 grid =
    let rec loop g acc =
        let nextGrid, removedCount = getNextGridAndRemovedCount g

        if removedCount > 0 then
            loop nextGrid (acc + removedCount)
        else
            acc

    loop grid 0

part2 parsed |> log
