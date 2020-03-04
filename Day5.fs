module Day5

type private Instruction = { opcode: int; p1mode: int; p2mode: int; p3mode: int; }

// Breaks an integer down into an opcode.
let private toInstruction num =
    let rec divide den' res num' =
        if num' = 0 then Array.ofList (List.rev res)
        else divide 10 ((num' % den')::res) (num'/den')
    let parts = divide 100 [] num
    // printfn "Num: %d Opcode: %A" num parts
    { opcode = Array.item 0 parts;
      p1mode = Option.defaultValue 0 (Array.tryItem 1 parts);
      p2mode = Option.defaultValue 0 (Array.tryItem 2 parts);
      p3mode = Option.defaultValue 1 (Array.tryItem 3 parts) }
    
let private getPos pos mode input =
    match mode with
    | 0 -> Array.item (Array.item pos input) input
    | 1 -> Array.item pos input
    | _ -> failwith "Invalid instruction mode"

// Applies the operator to the two items following readIndex and returns the result and the index to save the result at.
let private operate op (p1, mode1) (p2, mode2) (p3, mode3) input =
    let (x, y, s) = (getPos p1 mode1 input,
                     getPos p2 mode2 input,
                     getPos p3 mode3 input)
    (op x y, s)

// Traverses the input and applies the operators till the machine halts or an error occurs.
let rec private part1 inputVal input readIndex =
    let mutable input' = Array.copy input
    let instruction = toInstruction (Array.item readIndex input')
    match instruction with
    | { opcode = 1; p1mode = p1mode; p2mode = p2mode; p3mode = p3mode } ->
        printfn "Operating: %A" (input'.[readIndex..readIndex+3])
        let (res, pos) = operate (+) (readIndex+1, p1mode) (readIndex+2, p2mode) (readIndex+3, p3mode) input'
        // printfn "Pos: %d" pos
        input'.[pos] <- res
        part1 inputVal input' (readIndex + 4)
    | { opcode = 2; p1mode = p1mode; p2mode = p2mode; p3mode = p3mode } -> 
        printfn "Operating: %A" (input'.[readIndex..readIndex+3])
        let (res, pos) = operate (*) (readIndex+1, p1mode) (readIndex+2, p2mode) (readIndex+3, p3mode) input'
        input'.[pos] <- res
        part1 inputVal input' (readIndex + 4)
    | { opcode = 3; p1mode = _; p2mode = _; p3mode = _ } ->
        // 1 is the default input instruction for this problem
        printfn "Operating: %A" (input'.[readIndex..readIndex+1])
        let writeIndex = Array.item (readIndex + 1) input'
        input'.[writeIndex] <- inputVal
        part1 inputVal input' (readIndex + 2)
    | { opcode = 4; p1mode = p1mode; p2mode = _; p3mode = _ } ->
        printfn "Operating: %A" (input'.[readIndex..readIndex+1])
        let output = getPos (readIndex + 1) p1mode input'
        // let writeIndex = Array.item (readIndex + 1) input'
        printfn "Output from instruction 4: %d" output
        part1 inputVal input' (readIndex + 2)
    | { opcode = 5; p1mode = p1mode; p2mode = p2mode; p3mode = _ } -> 
        printfn "Operating: %A" (input'.[readIndex..readIndex+1])
        let (p1, p2) = (getPos (readIndex+1) p1mode input'), (getPos (readIndex+2) p2mode input')
        if p1 <> 0 then part1 inputVal input' p2
        else part1 inputVal input' (readIndex + 3)
    | { opcode = 6; p1mode = p1mode; p2mode = p2mode; p3mode = _ } -> 
        printfn "Operating: %A" (input'.[readIndex..readIndex+1])
        let (p1, p2) = (getPos (readIndex+1) p1mode input'), (getPos (readIndex+2) p2mode input')
        if p1 = 0 then part1 inputVal input' p2
        else part1 inputVal input' (readIndex + 3)
    | { opcode = 7; p1mode = p1mode; p2mode = p2mode; p3mode = p3mode } -> 
        printfn "Operating: %A" (input'.[readIndex..readIndex+3])
        let (res, pos) = operate (fun x y -> if x < y then 1 else 0) (readIndex+1, p1mode) (readIndex+2, p2mode) (readIndex+3, p3mode) input'
        input'.[pos] <- res
        part1 inputVal input' (readIndex + 4)
    | { opcode = 8; p1mode = p1mode; p2mode = p2mode; p3mode = p3mode } -> 
        printfn "Operating: %A" (input'.[readIndex..readIndex+3])
        let (res, pos) = operate (fun x y -> if x = y then 1 else 0) (readIndex+1, p1mode) (readIndex+2, p2mode) (readIndex+3, p3mode) input'
        input'.[pos] <- res
        part1 inputVal input' (readIndex + 4)
    | { opcode = 99; p1mode = _; p2mode = _; p3mode = _ } -> input
    | _ -> failwith ("Invalid opcode: " + (string instruction.opcode))

// Day 2 part 1 and 2.
let solve filePath =
    let input = 
        Utils.readAndMap ( fun (str: string) -> str.Split(",") |> Array.map int ) filePath
        |> Seq.head
    
    let mutable input' = Array.copy input
    let sol = part1 1 input' 0 |> Array.item 0
    let mutable input'' = Array.copy input
    let sol2 = part1 5 input'' 0 |> Array.item 0
    (string sol, string sol2)