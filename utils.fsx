module Utils

let log x =
    printfn "%A" x
    x // return value so it works in pipelines
