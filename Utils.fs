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