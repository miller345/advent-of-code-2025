// is it invalid (part 1)
let isInvalid_1 (id: int64) =
    let str = id.ToString()

    if str.Length % 2 = 1 then
        false
    else
        let mid = str.Length / 2
        str.[0 .. mid - 1] = str.[mid..]

// get the next invalid id greater than the given arg (part 1)
let getNextInvalid_1 (from: int64) =
    let str = from.ToString()

    if str.Length % 2 = 0 then
        let mid = str.Length / 2
        let a = str.[0 .. mid - 1]
        let invalidId = int64 (a + a)

        if invalidId > from then
            invalidId
        else
            let next = (int64 a) + (int64 1)
            int64 (next.ToString() + next.ToString())
    else
        "1" + String.replicate (((str.Length + 1) / 2) - 1) "0"
        |> String.replicate 2
        |> int64

// is it invalid (part 2)
let isInvalid_2 (id: int64) =
    let str = id.ToString()

    let possiblePatterns =
        [ 1 .. str.Length / 2 ]
        |> List.map (fun n -> str.[0 .. n - 1])
        |> List.map (fun h -> String.replicate (str.Length / h.Length) h)

    List.contains str possiblePatterns

// get the next invalid id greater than the given arg (part 2)
let rec getNextInvalid_2 (from: int64) =
    let x = from + int64 1
    if isInvalid_2 x then x else getNextInvalid_2 x

// find all invalid ids between given range (generic)
let findInvalidIds ((rangeFrom, rangeTo): int64 * int64, isInvalid: int64 -> bool, getNextInvalid: int64 -> int64) =
    let rec loop (invalidIds: int64 array) =
        if invalidIds.Length = 0 && isInvalid rangeFrom then
            loop [| rangeFrom |]
        else
            let nextInvalidId =
                match invalidIds.Length with
                | 0 -> getNextInvalid rangeFrom
                | _ -> getNextInvalid invalidIds.[invalidIds.Length - 1]

            if nextInvalidId >= rangeFrom && nextInvalidId <= rangeTo then
                loop (Array.append invalidIds [| nextInvalidId |])
            else
                invalidIds

    loop [||]

// find all invalid ids between given range (part 1)
let findInvalidIds_1 range =
    findInvalidIds (range, isInvalid_1, getNextInvalid_1)

// find all invalid ids between given range (part 2)
let findInvalidIds_2 range =
    findInvalidIds (range, isInvalid_2, getNextInvalid_2)

let parsed =
    System.IO.File.ReadAllText("2.txt").Split(",")
    |> Array.toList
    |> List.map (fun s -> s.Split("-") |> Array.map int64 |> (fun arr -> arr.[0], arr.[1]))

let partOne =
    parsed |> List.map findInvalidIds_1 |> List.toArray |> Array.concat |> Array.sum

printfn "partOne: %i" partOne

let partTwo =
    parsed |> List.map findInvalidIds_2 |> List.toArray |> Array.concat |> Array.sum

printfn "partTwo: %i" partTwo
