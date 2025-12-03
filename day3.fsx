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
