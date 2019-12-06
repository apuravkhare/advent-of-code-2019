// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let id = Option.defaultValue "1" ( Array.tryItem 0 argv )
    let file = Option.defaultValue (sprintf "inputs/day-%s.txt" id) (Array.tryItem 1 argv)
    let solver = RunnerHelper.solver id
    printfn "%O" (solver file)
    0 // return an integer exit code
