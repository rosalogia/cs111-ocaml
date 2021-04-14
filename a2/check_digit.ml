open Either (* contains partition_map *)

(* Unfortunately there is no stdlib
 * function to convert a string to
 * a list of characters. *)
let explode s =
    List.init (String.length s) (String.get s)

(* We slightly modify the List module
 * to include a List.sum function *)
module List = struct
    include List
    
    let sum = List.fold_left (+) 0
end

let () =
    Sys.argv.(1)                                    (* Take the number we got as a string ... *)
    |> explode                                      (* and convert it to a list of characters *)
    |> List.mapi (fun index character ->            (* Transform the list so that it contains indices ... *) 
            (index, int_of_char character - 48))    (* ... and the actual numbers the chars represent, since int_of_char returns an ascii code. *)
    |> List.partition_map (fun (i, c) ->            (* Partition the list such that even index numbers go in the left list and odd index numbers go*)
            if i mod 2 = 0 then                     (* in the right list. Using partition_map instead of plain old partition lets us get rid of the*)
                Left c                              (* index we were previously storing alongside the integer values in a tuple. *)
            else
                Right c)
    |> (fun (a, b) ->
            let evens =
                List.sum a mod 10 * 3 mod 10 in     (* Calculate the sum of the left list mod 10 * 3 mod 10 as instructed by the assignment *)
            let odds  =
                List.sum b mod 10 in                (* Perform a similar calculation for the right list *)
            (evens + odds) mod 10)                  (* and finally sum the two values we get and mod their sum by 10 *)
    |> Printf.printf "%d\n"                         (* Print the result with a newline *)

