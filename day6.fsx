#load "utils.fsx"
open Utils

let grid =
    let parseLine (str: string) = str |> Seq.toArray |> Array.map string
    let parsed = System.IO.File.ReadAllLines "./inputs/6.txt" |> Array.map parseLine
    let width = parsed.[0].Length
    let height = parsed.Length
    let grid = Array2D.init height width (fun y x -> parsed.[y].[x])
    grid


let rowLength = grid.[0, *].Length
let cols = [ 0 .. rowLength - 1 ] |> List.map (fun i -> grid.[*, i])

let blankColIndexes =
    cols
    |> List.map (Array.tryFind (fun s -> s <> " "))
    |> List.mapi (fun i x ->
        match x with
        | None -> Some i
        | _ -> None)
    |> List.choose id


let indexRanges =
    blankColIndexes @ [ rowLength ]
    |> List.mapi (fun i x ->
        let prev = if i = 0 then 0 else blankColIndexes.[i - 1]
        prev, x)

let groups =
    indexRanges
    |> List.map (fun (a, b) ->
        [ 0 .. Array2D.length1 grid - 1 ]
        |> List.map (fun y -> [ a .. b - 1 ] |> List.map (fun x -> grid.[y, x]))
        |> List.map (String.concat "")
        |> List.map (fun s -> s.Trim()))

let processGroup (group: string list) =
    let nums = group.[0 .. group.Length - 2] |> List.map int64
    let op = group[group.Length - 1]

    match op with
    | "+" -> nums |> List.sum
    | "*" -> nums |> List.fold (*) 1


// part 1
groups |> List.map processGroup |> List.sum |> log
