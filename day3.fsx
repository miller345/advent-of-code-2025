#load "utils.fsx"
open Utils

let parseLine (str: string) =
    str |> Seq.toArray |> Array.map string |> Array.map int

let findMaxJoltage (arr: int array) =
    let xMax = arr.[0 .. arr.Length - 2] |> Array.max
    let xPos = Array.findIndex ((=) xMax) arr
    let yMax = arr.[xPos + 1 ..] |> Array.max
    (xMax * 10) + yMax

System.IO.File.ReadAllLines("3.txt")
|> Array.map parseLine
|> Array.map findMaxJoltage
|> Array.sum
|> log

let findNextDigit (arr: int array, targetLength: int) =
    let len = arr.Length
    let reservedTailCount = targetLength - 1 // how many items must i save towards the end of the array
    let head = arr.[0 .. len - reservedTailCount - 1] // what am i left with
    let max = head |> Array.max // find the max
    let pos = Array.findIndex ((=) max) arr
    let tail = arr.[pos + 1 ..] // new array following on from selected max
    max, tail

let findMaxJoltage_p2 (arr: int array, targetLength: int) =
    let rec loop (str: string, tail) =
        if str.Length = targetLength then
            int64 (str)
        else
            // let x = log (str, tail)
            let digit, remaining = findNextDigit (tail, targetLength - str.Length)

            if remaining.Length > 0 then
                loop (str + digit.ToString(), remaining)
            else
                int64 (str + digit.ToString())

    loop ("", arr)


System.IO.File.ReadAllLines("3.txt")
|> Array.map parseLine
|> Array.map (fun arr -> findMaxJoltage_p2 (arr, 12))
|> Array.sum
|> log
