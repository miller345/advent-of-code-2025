// parse as integer (where negative is anti-clockwise)
let parseLine (s: string) =
    match s with
    | s when s.StartsWith "L" -> s.Substring 1 |> int |> (fun x -> -x)
    | s when s.StartsWith "R" -> s.Substring 1 |> int
    | _ -> failwith "Unexpected value"

// given a starting point, return the result of an integer twist
let twist from by = (((from + by) % 100) + 100) % 100

let parsed = System.IO.File.ReadAllLines "./inputs/1.txt" |> Array.map parseLine

let partOne = parsed |> Array.scan twist 50 |> Array.filter ((=) 0) |> Array.length

printfn "Part one: %i" partOne

// how many times does it land on zero during a twist
let landsOnZero from by =
    if by > 0 then // clockwise
        let toReachZero = 100 - from

        if by > toReachZero then ((by - toReachZero) / 100) + 1
        elif by < toReachZero then 0
        else 1
    elif by < 0 then // anti clockwise
        let toReachZero = -from

        if by > toReachZero then
            0
        elif by < toReachZero then
            (if from = 0 then 0 else 1) - ((by - toReachZero) / 100)
        else
            1
    else
        0


let p2 (from, acc) by = (twist from by, landsOnZero from by)

let partTwo =
    parsed
    |> Array.scan p2 (50, 0)
    |> Array.map (fun (_, zeros) -> zeros)
    |> Array.sum

printfn "Part two: %i" partTwo
