module RunnerHelper

let solver id =
    match id with
    | "1" -> Day1.solve
    | "2" -> Day2.solve
    | "3" -> Day3.solve
    | "4" -> Day4.solve
    | "5" -> Day5.solve
    | "6" -> Day6.solve
    | _ -> failwith (sprintf "No matching solver for day %s." id)

