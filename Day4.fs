module Day4

// Checks if a number satisfies the increasing and repetition criterion (injectable).
let private isValid repCondition num =
    let nums =
        System.Text.RegularExpressions.Regex.Split((string num), "")
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map int
    let (_, isIncreasing) = Array.fold ( fun (prev, valid) current -> (current, valid && (current >= prev)) ) (-1, true) nums
    let hasDouble =
        nums
        |> Array.groupBy id
        |> Array.exists ( fun (_, v) -> repCondition (Array.length v) )
    isIncreasing && hasDouble

// For every number between first and last, finds the count of numbers that satisfy the validity criterion.
let private solve' first last validator repCondition = 
    [first..last]
    |> List.filter (validator repCondition)
    |> List.length

// Day 4 Part 1 and 2.
let solve input =
    let file = System.IO.File.ReadLines(input) |> Seq.head
    let m = System.Text.RegularExpressions.Regex.Match(file, "^(\\d+)-(\\d+)$")
    let first = int (m.Groups.[1].Value)
    let last = int (m.Groups.[2].Value)
    let p1 = solve' first last isValid (fun i -> i > 1)
    let p2 = solve' first last isValid (fun i -> i = 2)
    (string p1, string p2)