module Utils

open System.Collections.Generic

// Reads a file into lines, and applies the mapping function to each line.
let readAndMap f filePath = 
    System.IO.File.ReadAllLines filePath
    |> Seq.map f

// Returns a memoized version of a function.
let memoize f =
    let cache = Dictionary<_,_>()
    fun x ->
        if cache.ContainsKey(x) then cache.[x]
        else
            let res = f(x)
            cache.[x] <- res
            res

// Applies a function to the input recursively till a predicate is satisfied; aggregates the results using an input function.
let rec applyByPredicate f agg pred acc ip =
    let res = f ip
    if pred res then acc
    else applyByPredicate f agg pred (agg acc res) res

// Calculates the manhattan distance between two 2d points
let manhattan2d p1 p2 =
    abs ((fst p1) - (fst p2)) + abs ((snd p1) - (snd p2))


let filterAndPairBy f s1 s2 = 
    Seq.map ( fun i1 -> match Seq.tryFind (f i1) s2 with
                        | Some i2 -> Some [i1; i2]
                        | None -> None ) s1
    |> Seq.filter Option.isSome
    |> Seq.map Option.get

let rec foldAndPick folder picker acc1 acc2 s1 s2 =
    match s1, s2 with
    | [], _ -> None
    | _, [] -> None
    | x::xs, y::ys -> match picker acc1 acc2 with
                      | Some res -> Some res
                      | None -> foldAndPick folder picker (List.append acc1 (folder acc1 x)) (List.append acc2 (folder acc2 y)) xs ys