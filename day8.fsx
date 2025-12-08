#load "utils.fsx"
open Utils

type Point = int * int * int

let parseLine (str: string) : Point =
    str.Split ","
    |> Array.map int
    |> function
        | [| a; b; c |] -> a, b, c
        | _ -> failwith "unexpected input"

let parsed =
    System.IO.File.ReadAllLines "./inputs/8.eg.txt"
    |> Array.map parseLine
    |> Array.toList

let getDistance ((ax, ay, az): Point) ((bx, by, bz): Point) =
    sqrt (float (ax - bx) ** 2 + float (ay - by) ** 2 + float (az - bz) ** 2)

/// compare a single point with a list of points, return the nearest point (and the distance)
let getNearest (points: Point list) (p: Point) =
    points
    |> List.map (fun pp -> pp, getDistance p pp)
    |> List.minBy (fun (_, d) -> d)

/// given a list of points, get the closest pair
let getClosestPair (ps: Point list) (exclusions: (Point * Point) list) =
    let rec loop (points: Point list) (pairs: (Point * Point * float) list) =
        match points with
        | [] -> pairs
        | head :: tail ->
            let filteredTail =
                tail
                |> List.filter (fun p -> not (List.contains (head, p) exclusions || List.contains (p, head) exclusions))

            let nearest, distance = getNearest filteredTail head
            let remaining = tail |> List.filter (fun x -> x <> nearest)
            loop remaining ((head, nearest, distance) :: pairs)

    loop ps [] |> List.minBy (fun (_, _, d) -> d) |> (fun (a, b, _) -> a, b)


let getShortestConnections (ps: Point list) =
    let rec loop (points: Point list) (pairs: (Point * Point) list) =
        if pairs.Length * 2 = points.Length then
            pairs
        else
            let a, b = getClosestPair points pairs
            (a, b) |> log
            loop points ((a, b) :: pairs)

    loop ps []

parsed |> getShortestConnections


// 162,817,812 and 425,690,689
// 162,817,812 and 431,825,988
// 906,360,560 and 805,96,715
// 431,825,988 and 425,690,689 - these two junction boxes were already in the same circuit, nothing happens!
