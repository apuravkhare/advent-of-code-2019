module Day1

// Maps the input integers using the specified computation (divide by 3, subtract 2).
let private memoizedMap =
    Utils.memoize ( fun x -> ( x / 3 ) - 2 )

// Returns the sum of mapping each input number using the specified computation.
let private part1 input =
    input
    |> Seq.sumBy memoizedMap

// Returns the sum of mapping each input recursively using the part1 computation.
let private part2 input =
    input
    |> Seq.sumBy ( fun x -> Utils.applyByPredicate memoizedMap (+) ( fun r -> r <= 0 ) 0 x )

// Day 1 - part 1 and 2 solution.
let solve filePath =
    let input = Utils.readAndMap int filePath
    (string (part1 input), string (part2 input))