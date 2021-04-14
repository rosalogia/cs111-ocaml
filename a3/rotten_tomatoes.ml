(* We write a function for splitting a long list
 * into a list of equally sized sub-lists. E.g.
 *
 * [1; 2; 3; 4; 5; 6; 7; 8; 9] becomes
 * [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] 
 *
 * We achieve this by folding over the list with
 * a triple containing the amount of elements remaining
 * to be added to a sub-list, the current sub-list, and
 * the list of sub-lists we're building.
 *
 * For every new element of the list that we're adding
 * to our list of sub-lists, we have to choose to either
 * continue adding it to the current sub-list or to move
 * a finished sub-list to the list of sub-lists, and add
 * the new value to a new sub-list. Take your time parsing
 * this. It's a little heavy but tracing through it helps. *)
let split n l =
    let aux (rem, cur, res) x =
        if rem = 0 then 
            (* If remaining = 0, we want to put the new
             * list value in a new sub-list, and add
             * the now complete sub-list from before to
             * the list of resultant sub-lists *)
            (n - 1, [x], res @ [cur])
        else
            (* Otherwise, we can add the new list value into
             * the sub-list we're currently building, and
             * decrement the remaining count. *)
            (rem - 1, cur @ [x], res) in
  let (_, remaining_sublist, sublists) = List.fold_left aux (n, [], []) l in
  (* In case the length of l doesn't divide evenly by n,
   * we append a list containing an empty or unfinished
   * sub-list to our list of sublists *)
  sublists @ [remaining_sublist]

(* We add some convenience functions to the List module
 * to prevent our pipelines from getting too messy *)
module List = struct
    include List
    let rec range a b =
        if a >= b then
            []
        else
            a :: range (a+1) b
    
    let sortBy f =
        List.sort (fun x y ->
            if f x = f y then
                0
            else if f x > f y then
                1
            else
                -1)

    let sum = List.fold_left (+) 0

    let get n l = List.nth l n
end

let () =
    let number_of_movies = int_of_string Sys.argv.(2) in

    (* We wrote the split function so that we could split the list
     * of movie reviews into sub-lists whose size is determined
     * by the number of movies. Each sub-list represents a reviewer. *)
    let reviews =
        Array.sub Sys.argv 3 (Array.length Sys.argv - 3)
        |> Array.to_list
        |> List.map int_of_string
        |> split number_of_movies in

    (* Now we want to grab the sum of reviews for each movie
     * and find out which movie had the greatest summed reviews.
     * We begin by creating a list from 0 to the amount of movies - 1
     * in order to index into our lists. *)
    List.range 0 number_of_movies
    |> List.map (fun i ->   (* We map over the list of indices in an interesting way: *)
            let rating_sum =    
                reviews                     (* We use the index to map over the list of list of reviews while *)
                |> List.map (List.get i)    (* only grabbing the review for the movie with the index we care about. *)
                |> List.sum in              (* Then we sum those reviews *)
            (i, rating_sum))                (* and return the sum along with the corresponding movie index. *)
    |> List.sortBy snd                      (* We sort by the sum (the second element of the tuple) *)
    |> List.rev                             (* Reverse because of course sorting is ascending by default *)
    |> List.hd                              (* Grab the head, which should be the element with the largest sum *)
    |> fst                                  (* And only grab its index, because that's all we have to output *)
    |> Printf.printf "%d\n"
