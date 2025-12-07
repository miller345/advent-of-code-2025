#load "utils.fsx"
open Utils

type Grid = char array2d

let parsed =
    let parseLine (str: string) = str |> Seq.toArray // |> Array.map string
    let parsed = System.IO.File.ReadAllLines "./inputs/7.txt" |> Array.map parseLine
    let width = parsed.[0].Length
    let height = parsed.Length
    let grid = Array2D.init height width (fun y x -> parsed.[y].[x])
    grid

let getRow y (grid: Grid) = grid.[y, *]
let getCol x (grid: Grid) = grid.[*, x]

let findS (grid: Grid) =
    grid |> getRow 0 |> Array.findIndex ((=) 'S')

let getColFrom y x (grid: Grid) =
    grid |> (getCol x) |> (fun arr -> arr.[y..])

let findNextSplitter (col: char array) = col |> Array.tryFindIndex ((=) '^')

let splitBeamAt y x = (y, x - 1), (y, x + 1)

/// drop beam from point and return the split point
let dropBeam y x (grid: Grid) =
    grid
    |> getColFrom y x
    |> findNextSplitter
    |> fun yy ->
        match yy with
        | None -> None
        | Some yy -> Some(yy + y, x)

/// drop the given beams, return resulting split points
let dropBeams (beams: (int * int) list) (grid: Grid) =
    beams |> List.map (fun (y, x) -> dropBeam y x grid) |> List.choose id

let part1 (grid: Grid) =
    let rec loop (beams: (int * int) list) acc =
        let splitPoints = grid |> dropBeams beams

        let nextBeams =
            splitPoints
            |> List.map (fun (y, x) -> splitBeamAt y x)
            |> List.map (fun (a, b) -> [ a; b ])
            |> List.concat
            |> List.distinct

        match nextBeams with
        | [] -> acc |> List.length
        | _ -> loop nextBeams ((acc @ splitPoints) |> List.distinct)

    loop [ (0, findS grid) ] []

parsed |> part1 |> log
