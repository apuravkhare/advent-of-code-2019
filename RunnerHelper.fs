module RunnerHelper

let solver id =
    match id with
    | "1" -> Day1.solve
    | "2" -> Day2.solve
    | _ -> failwith (sprintf "No matching solver for day %s." id)

