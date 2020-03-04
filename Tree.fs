module Tree

type Tree<'a> = | Node of 'a * Tree<'a> list

let rec private getNode tree data =
    let (Node(nodeData, children)) = tree
    if nodeData = data then Some tree
    else
        let rec findInChildren children =
            match children with
            | [] -> None
            | child::children' ->
                match getNode child data with
                | Some matched -> Some matched
                | None -> findInChildren children'
        findInChildren children

let rec ancestors tree node acc =
    let (Node(nodeData, children)) = tree
    if nodeData = node then Some acc
    else
        let rec findInChildren children acc =
            match children with
            | [] -> None
            | child::children' ->
                match ancestors child node (child::acc) with
                | Some matched -> Some matched
                | None -> findInChildren children' []
        findInChildren children acc

let rec construct map tree =
    match map with
    | [] -> tree
    | (k, v)::kvs ->
        let child = Option.defaultValue (Node(v, [])) (List.tryFind (fun (Node(data ,_)) -> data = v) tree)
        let parent = Option.defaultValue (Node(k, [])) (List.tryFind (fun (Node(data ,_)) -> data = k) tree)
        let (Node(parentData, children)) = parent
        let parent' = Node(parentData, child::children)
        construct kvs (parent'::(List.except [parent] tree))

(*
let rec getParentOf node tree =
    match tree with
    | Node (_, children) ->
        if Array.contains node children then
            tree
        else
            match children with
            | [] -> 

*)