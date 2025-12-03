#load "utils.fsx"
open Utils

let findNextDigit (arr: int array, targetLength: int) =
    let len = arr.Length
    let reservedTailCount = targetLength - 1 // how many items must i save towards the end of the array
    let head = arr.[0 .. len - reservedTailCount - 1] // what am i left with
    let max = head |> Array.max // find the max
    let pos = Array.findIndex ((=) max) arr
    let tail = arr.[pos + 1 ..] // new array following on from selected max
    max, tail

let findMaxJoltage (arr: int array, targetLength: int) =
    let rec loop (str: string, tail) =
        if str.Length = targetLength then
            int64 str
        else
            let digit, remaining = findNextDigit (tail, targetLength - str.Length)
            loop (str + digit.ToString(), remaining)

    loop ("", arr)


let parseLine (str: string) =
    str |> Seq.toArray |> Array.map string |> Array.map int

let parsed = System.IO.File.ReadAllLines("3.txt") |> Array.map parseLine

// part 1
parsed |> Array.map (fun arr -> findMaxJoltage (arr, 2)) |> Array.sum |> log
// part 2
parsed |> Array.map (fun arr -> findMaxJoltage (arr, 12)) |> Array.sum |> log

// 17034
// 168798209663590L
