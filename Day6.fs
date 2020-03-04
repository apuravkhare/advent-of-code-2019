module Day6

open System.Collections.Generic
open Tree

let private part1 input =
    (*
    let map = Dictionary<string, string[]>()
    for (key, value) in input do
        if map.ContainsKey(key) then
            map.[key] <- Array.append map.[key] [|value|]
        else
            map.Add(key, [|value|])
    let kvps = Seq.map (fun (kvp: KeyValuePair<_,_>) -> (kvp.Key, kvp.Value)) map
               |> List.ofSeq
    *)
    Tree.construct input []
    


let solve filePath =
    let input = 
        Utils.readAndMap ( fun (str: string) -> str.Split(")") |> fun arr -> (Array.head arr, Array.last arr) ) filePath
        |> List.ofSeq
    let sol1 = part1 input
    (sprintf "%A" sol1, "")