module Day2

// Applies the operator to the two items following readIndex and returns the result and the index to save the result at.
let private operate op readIndex input =
    let (x, y, s) = (Array.item (Array.item (readIndex + 1) input) input,
                     Array.item (Array.item (readIndex + 2) input) input,
                     Array.item (readIndex + 3) input)
    (op x y, s)

// Traverses the input and applies the operators till the machine halts or an error occurs.
let rec private part1 input readIndex =
    let mutable input' = Array.copy input
    match Array.item readIndex input with
    | 1 -> let (res, pos) = operate (+) readIndex input
           input'.[pos] <- res
           part1 input' (readIndex + 4)
    | 2 -> let (res, pos) = operate (*) readIndex input
           input'.[pos] <- res
           part1 input' (readIndex + 4)
    | 99 -> input
    | _ -> failwith "Invalid opcode"

// Finds a combination of inputs at position 1 and 2 that results in '19690720' at position 0 when the machine halts.
// TODO: Try a non-brute force solution
let private part2 (input: int[]) =
    let mutable sol = (0, 0)
    for i in [0..99] do
        for j in [0..99] do
            let mutable input' = Array.copy input
            input'.[1] <- i
            input'.[2] <- j
            let res =
                try
                    Array.item 0 (part1 input' 0)
                with
                | _ -> 0
            if res = 19690720 then
                sol <- (i, j)
            else ignore None
    ((fst sol) * 100) + (snd sol)

// Day 2 part 1 and 2.
let solve filePath =
    let input = 
        Utils.readAndMap ( fun (str: string) -> str.Split(",") |> Array.map int ) filePath
        |> Seq.head
    
    let mutable input' = Array.copy input
    input'.[1] <- 12
    input'.[2] <- 2
    let sol = part1 input' 0 |> Array.item 0
    (string sol, string (part2 input))