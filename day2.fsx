let log x =
    printfn "%A" x
    x // return value so it works in pipelines


let isInvalidId (id: int64) =
    let str = id.ToString()

    if str.Length % 2 = 1 then
        false
    else
        let mid = str.Length / 2
        str.[0 .. mid - 1] = str.[mid..]

// get the next invalid id greater than the given arg
let getNextInvalidId (from: int64) =
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

let rec findInvalidIds ((rangeFrom, rangeTo): int64 * int64, invalidIds: int64 array) =
    if invalidIds.Length = 0 && isInvalidId rangeFrom then
        findInvalidIds ((rangeFrom, rangeTo), [| rangeFrom |])
    else
        let nextInvalidId =
            match invalidIds.Length with
            | 0 -> getNextInvalidId rangeFrom
            | _ -> getNextInvalidId invalidIds.[invalidIds.Length - 1]

        if nextInvalidId >= rangeFrom && nextInvalidId <= rangeTo then
            findInvalidIds ((rangeFrom, rangeTo), Array.append invalidIds [| nextInvalidId |])
        else
            invalidIds


let partOne =
    System.IO.File.ReadAllText("2.txt").Split(",")
    |> Array.toList
    |> List.map (fun s -> s.Split("-") |> Array.map int64 |> (fun arr -> arr.[0], arr.[1]))
    |> List.map (fun range -> findInvalidIds (range, [||]))
    |> List.toArray
    |> Array.concat
    |> Array.sum

log partOne




let isInvalidId2 (id: int64) =
    let str = id.ToString()

    let possiblePatterns =
        [ 1 .. str.Length / 2 ]
        |> List.map (fun n -> str.[0 .. n - 1])
        |> List.map (fun h -> String.replicate (str.Length / h.Length) h)

    List.contains str possiblePatterns

let rec getNextInvalidId2 (from: int64) =
    let x = from + int64 1
    if isInvalidId2 x then x else getNextInvalidId2 x


let rec findInvalidIds2 ((rangeFrom, rangeTo): int64 * int64, invalidIds: int64 array) =
    if invalidIds.Length = 0 && isInvalidId2 rangeFrom then
        findInvalidIds2 ((rangeFrom, rangeTo), [| rangeFrom |])
    else
        let nextInvalidId =
            match invalidIds.Length with
            | 0 -> getNextInvalidId2 rangeFrom
            | _ -> getNextInvalidId2 invalidIds.[invalidIds.Length - 1]

        if nextInvalidId >= rangeFrom && nextInvalidId <= rangeTo then
            findInvalidIds2 ((rangeFrom, rangeTo), Array.append invalidIds [| nextInvalidId |])
        else
            invalidIds


let partTwo =
    System.IO.File.ReadAllText("2.txt").Split(",")
    |> Array.toList
    |> List.map (fun s -> s.Split("-") |> Array.map int64 |> (fun arr -> arr.[0], arr.[1]))
    |> List.map (fun range -> findInvalidIds2 (range, [||]))
    |> List.toArray
    |> Array.concat
    |> Array.sum

log partTwo
