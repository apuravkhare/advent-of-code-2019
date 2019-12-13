module Day3

// Helper type - identifies the direction.
type private Direction = | Left
                         | Right
                         | Up
                         | Down

// Helper type - a record of the direction and number of steps.
type private Coordinate = { dir: Direction; units: int; }

// Creates a Coordinate record from a string.
let private toCoordinate str =
    let m = System.Text.RegularExpressions.Regex.Match(str, "^(L|R|U|D)(\\d+)$")
    let dir = match m.Groups.[1].Value with
              | "L" -> Direction.Left
              | "R" -> Direction.Right
              | "U" -> Direction.Up
              | "D" -> Direction.Down
              | _ -> failwith "Invalid direction"
    { dir = dir; units = int(m.Groups.[2].Value) }

// Creates a path from a sequence of coordinates.
let private createPath coords = 
    let applyCoord prev coord =
        match coord.dir with
        | Left -> List.map ( fun i -> ((fst prev) - i, snd prev) ) [1..coord.units]
        | Right -> List.map ( fun i -> ((fst prev) + i, snd prev) ) [1..coord.units]
        | Up -> List.map ( fun i -> (fst prev, (snd prev) + i) ) [1..coord.units]
        | Down -> List.map ( fun i -> (fst prev, (snd prev) - i) ) [1..coord.units]
    coords
    |> Seq.fold ( fun acc coord -> List.append acc (applyCoord (List.last acc) coord) ) [(0, 0)]
    |> List.except [(0, 0)]
    |> Set.ofList

// Finds the intersection closest to the origin among paths.
let private part1 input =
    let paths = Seq.map createPath input
    let intersects = Set.intersectMany paths
    intersects
    |> Seq.map ( fun p -> Utils.manhattan2d p (0, 0) )
    |> Seq.min

// Creates a path from a sequence of coordinates, that is aware of the number of steps it took to get to a position in the path.
let private createMeasuredPath coords =
    let applyCoord prev coord =
        let (x, y, steps) = prev
        match coord.dir with
        | Left -> List.map ( fun i -> (x - i, y, steps + i) ) [1..coord.units]
        | Right -> List.map ( fun i -> (x + i, y, steps + i) ) [1..coord.units]
        | Up -> List.map ( fun i -> (x, y + i, steps + i) ) [1..coord.units]
        | Down -> List.map ( fun i -> (x, y - i, steps + i) ) [1..coord.units]
    coords
    |> Seq.fold ( fun acc coord -> List.append acc (applyCoord (List.last acc) coord) ) [(0, 0, 0)]
    |> List.except [(0, 0, 0)]
    |> Set.ofList

// Not in use - Attempted optimized solution for part 2.
// It misses out a few cases because it assumes that the first intersection would be the quickest one.
let private _part2 input =
    let applyCoord prevs coord =
        let prev = List.last prevs
        let (x, y, steps) = prev
        match coord.dir with
        | Left -> List.map ( fun i -> (x - i, y, steps + i) ) [1..coord.units]
        | Right -> List.map ( fun i -> (x + i, y, steps + i) ) [1..coord.units]
        | Up -> List.map ( fun i -> (x, y + i, steps + i) ) [1..coord.units]
        | Down -> List.map ( fun i -> (x, y - i, steps + i) ) [1..coord.units]

    let tryIntersect s1 s2 =
        let set1 = s1 |> List.except [(0, 0, 0)]
        let set2 = s2 |> List.except [(0, 0, 0)]
        let mapped =
            List.map ( fun i1 -> match List.filter ( ( fun (x1,y1,_) (x2,y2,_) -> x1 = x2 && y1 = y2 ) i1 ) set2 with
                                 | [] -> None
                                 | ls -> Some ( List.map ( fun l -> (i1, l) ) ls ) ) set1
            |> List.filter Option.isSome
            |> List.map Option.get
        printfn "intersected: %A" mapped
        match List.isEmpty mapped with
        | true -> None
        | false -> Some (List.concat mapped)

    let p1::(p2::_) = input |> Seq.map List.ofSeq |> List.ofSeq
    let intersected = Utils.foldAndPick applyCoord tryIntersect [(0, 0, 0)] [(0, 0, 0)] p1 p2
    let min =
        Option.get intersected
        |> Seq.minBy ( fun ((_,_,s1),(_,_,s2)) -> s1+s2 )
    
    let ((_,_,s1),(_,_,s2)) = min
    s1+s2

// Finds the intersection among paths that took the least number of combined steps to get to.
let private part2 input =
    let path = Seq.map createMeasuredPath input |> List.ofSeq
    let p1::(p2::_) = path 
    let intersected = Utils.filterAndPairBy ( fun (x1,y1,_) (x2,y2,_) -> x1 = x2 && y1 = y2 ) p1 p2 |> List.ofSeq
    let min =
        intersected
        |> List.minBy ( fun ((_,_,s1)::((_,_,s2)::_)) -> s1+s2 )
    let ((_,_,s1)::((_,_,s2)::_)) = min
    s1+s2

// Day 3 part 1 and 2.
// TODO: Need to come back to this and try a more optimal solution.
let solve filePath =
    let input = Utils.readAndMap ( fun (str: string) -> str.Split(",") |> Seq.map toCoordinate ) filePath
    (string (part1 input), string (part2 input))