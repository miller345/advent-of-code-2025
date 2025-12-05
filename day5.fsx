#load "utils.fsx"
open Utils

type Range = int64 * int64

let isInRange ((rangeFrom, rangeTo): Range) (ingredient: int64) =
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

let doRangesOverlap (r1: Range) (r2: Range) =
    let r1f, r1t = r1
    let r2f, r2t = r2
    isInRange r2 r1f || isInRange r2 r1t || isInRange r1 r2f || isInRange r1 r2t

let combineRanges (r1: Range) (r2: Range) =
    let r1f, r1t = r1
    let r2f, r2t = r2
    min r1f r2f, max r1t r2t

let normaliseRanges (ranges: Range list) =
    let rec loop (rs: Range list) (acc: Range list) =
        match rs with
        | [] -> acc
        | head :: tail ->
            let overlaps, noOverlap = tail |> List.partition (doRangesOverlap head)

            match overlaps with
            | [] -> loop noOverlap (head :: acc)
            | _ ->
                let combined = overlaps |> List.fold combineRanges head
                loop (combined :: noOverlap) acc


    loop ranges []

let partTwo (ranges, _) =
    ranges |> normaliseRanges |> List.map (fun (rf, rt) -> rt - rf + 1L) |> List.sum

parsed |> partTwo |> log
