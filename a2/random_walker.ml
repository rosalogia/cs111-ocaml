let step (x, y) =
    (* Match on a random integer between 0 and 3
     * incrementing or decrementing either x or y
     * depending on the result. The pattern matching
     * is not exhaustive, but because we know
     * Random.int 4 can only return a number between
     * 0 and 3, this is okay. *)
    match Random.int 4 with
    | 0 -> (x + 1, y)
    | 1 -> (x - 1, y)
    | 2 -> (x, y + 1)
    | 3 -> (x, y - 1)

(* We define some simple functions to make working
 * with ordered pairs of numbers a little easier *)
let print_tuple (x, y) = Printf.printf "(%d, %d)\n" x y

let combine_tuple (a, b) (x, y) = (a + x, b + y)

(* scanl is a function that exists in the Haskell
 * standard library's List module. Read about it here:
 * https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#g:6 *)
let rec scanl f init l =
    init :: match l with
            | [] -> []
            | h::t -> scanl f (f init h) t

let () =
    (* Initialise the random number generator *)
    Random.self_init ();

    (* Define the initial position and read in the number
     * of steps we'll be taking from the command-line args *)
    let initial_pos = (0, 0) in
    let number_of_steps = int_of_string Sys.argv.(1) in

    let steps =
        (* We begin by creating a list of steps on the position (0, 0),
         * so in effect we should have a list that looks something like
         * [(1, 0); (-1, 0); (0, 1); (0, -1); ...] *)
        List.init number_of_steps (fun _ -> step (0, 0))
        (* Then we use scanl to start from our initial position
         * and add all our steps to the initial position *)
        |> scanl combine_tuple initial_pos in  
    
    (* Because of how scanl works, we now have a list beginning with our initial pos where every element
     * afterwards contains the accumulated result of applying each step to the initial position, so we can
     * easily print them all out on their own lines. *)
    List.iter print_tuple steps;    

    steps
    |> List.rev (* Your favourite O(n) way of getting the last element of a list *)
    |> List.hd
    |> (fun (x, y) -> float_of_int x ** 2. +. float_of_int y ** 2.)
    |> Printf.printf "Squared distance = %.1f\n" 
