type 'a node =
    Node of 'a * 'a node list
;;

let find_dfs start goal =
    let rec search_node (Node (el, next)) =
        let rec search_list = function
            | [] -> false
            | hd :: tl -> search_node hd || search_list tl
        in
        if el = goal then true
        else search_list next
    in
    search_node start
;;
assert (find_dfs (Node ('a', [Node ('b', []); Node ('c', [])])) 'c');;

let find_bfs start goal =
    let rec aux = function
        | [] -> false
        | Node (el, next) :: tl ->
                if el = goal then true
                else aux (tl @ next)
    in
    aux [start]
;;
assert (find_bfs (Node ('a', [Node ('b', []); Node ('c', [])])) 'c');;

