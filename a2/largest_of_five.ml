let max l =
    (* Define an auxiliary function
     * to find the maximum so that our
     * actual max function doesn't need
     * to take in a current max parameter *)
    let rec aux cmax l' =
        match l' with
        | [] -> cmax    (* If input list is empty, return current max parameter *)
        | h::t ->       (* If the input list still has a "head", i.e. a first element ... *)
                if h > cmax then    (* If the first element of the list > cmax ... *)
                    aux h t             (* Recursively call the aux function with the head as the new max *)
                else
                    aux cmax t          (* Otherwise, recursively call the aux function without changing the current max*)
    in
    aux Int.min_int l

(* This isn't necessary, but here's a function called maxBy that lets you define
 * how the max should be determined by taking a comparison function as input.
 * If we were to use it instead of "max", we would call it like so:
 * 
 * maxBy (>) Int.min_int [1; 2; 3; 4; 5]
 * 
 * *)
let maxBy p min l =
    let rec aux cmax l' =
        match l' with
        | [] -> cmax
        | h::t ->
                if p h cmax then
                    aux h t
                else
                    aux cmax t
    in
    aux min l

let () =
    (* Quick reminder as to how the pipe operator works:
     * If we have a function f that takes an input x, e.g.
     * f x = x + 1, then f x is equivalent to x |> f
     * This allows us to write out long chains of function
     * application without tons of nested parentheses. It's
     * also a little more clear how our input is being transformed
     * in order to achieve the objective of our program. *)
    Sys.argv                    
    |> Array.to_list            (* Convert command-line args to a string list           *)
    |> List.tl                  (* Get the list's "tail", i.e. remove the first element *)
    |> List.map int_of_string   (* Convert all the elements to integers                 *)
    |> max                      (* find the list's maximum                              *)
    |> Printf.printf "%d\n"     (* Output it with a new line                            *)
