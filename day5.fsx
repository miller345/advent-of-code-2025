#load "utils.fsx"
open Utils

let isInRange ((rangeFrom, rangeTo): int64 * int64) (ingredient: int64) =
    ingredient >= rangeFrom && ingredient <= rangeTo

let isFresh ranges ingredient =
    match List.tryFind (fun range -> isInRange range ingredient) ranges with
    | None -> false
    | _ -> true

let partOne (ranges, ingredients) =
    ingredients
    |> List.map (isFresh ranges)
    |> List.filter ((=) true)
    |> List.length

let parsed =
    let lines = System.IO.File.ReadAllLines "./inputs/5.txt"
    let index = Array.findIndex ((=) "") lines
    let rangeLines = lines[0 .. index - 1]
    let ingredientsLines = lines[index + 1 ..]

    let parseRangeLine (line: string) =
        line.Split "-" |> Array.map int64 |> (fun arr -> arr.[0], arr.[1])

    let ranges = rangeLines |> Array.map parseRangeLine |> Array.toList
    let ingredients = ingredientsLines |> Array.map int64 |> Array.toList
    ranges, ingredients

parsed |> partOne |> log
