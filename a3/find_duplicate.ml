let duplicates l =
    let rec aux seen l' =
        match l' with
        | [] -> false
        | h::t ->
                if List.mem h seen then
                    true
                else
                    aux (h::seen) t in
    aux [] l

let () =
    Sys.argv
    |> Array.to_list
    |> List.tl
    |> List.map int_of_string
    |> duplicates
    |> Printf.printf "%b\n"
