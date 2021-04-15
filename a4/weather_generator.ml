open Str

type weather =
    | Dry
    | Wet

let print_weather = function
    | Dry -> print_endline "Dry"
    | Wet -> print_endline "Wet"


(* We define a function that, given a file to read from, a location, and a month, produces
 * a result type containing the correct transition probability if it exists, and an error
 * message if it couldn't be found in the file. *)
let transition_probability longitude latitude month file =
    let input_channel = open_in file in (* Open an input channel on the file *)
    let rec aux () =    (* We define a recursive function that searches sequentially for the correct row of the table, *)
        let line =      (* then accesses the probability that corresponds to the "month" parameter *)
            try         (* First we try and read a line from the input channel. *)
                Ok (input_line input_channel)   (* If it works, wrap the line in Ok *)
            with End_of_file ->                 (* But, if it fails (we run out of lines) ... *)
                close_in input_channel;         (* Close the file ... *)
                Error "Invalid inputs or data file" in (* And return an Error. This will be processed next. *)
        match line with (* Now we process our result and proceed accordingly *)
        | Ok table_row -> ( (* If line matches the Ok ... pattern, we know that a line was successfully read. *)
                table_row (* Now we begin processing the row itself, which is currently a long string *)
                |> Str.split (Str.regexp "\t")  (* Since the rows are tab-separated, we split by the tab character *)
                |> function                     (* Now we pass the list of values in the row to a pattern match *)
                    | longitude :: latitude :: t -> (* If the first two values in the list are the correct longitude and latitude, *)
                            close_in input_channel; (* we close the input channel, *) 
                            Ok (List.nth t (month - 2)  (* and return the value in the list corresponding to the specified month, *)
                                |> float_of_string)     (* as a float, wrapped in Ok *)
                    | _ -> aux ())  (* If the correct longitude and latitude aren't the first two elements of the list, we recurse *)
        | Error m -> Error m in     (* If we ran into the end of the file, we just return an Error with the same message as before *)
    aux ()

let days_in_month = [| 31 ; 28 ; 31 ; 30 ; 31 ; 30 ; 31 ; 31 ; 30 ; 31 ; 30 ; 31 |]

(* We define a helper function for the one_month_generator
 * that generates the forecast for today based on the
 * probabilities being used and yesterday's weather *)
let gen_today dry_wet wet_wet yesterday =
    let chance = Random.float 1.0 in
    if yesterday = Wet then
        if chance <= wet_wet then Wet else Dry
    else
        if chance <= dry_wet then Wet else Dry


let one_month_generator longitude latitude month =
    (* We begin by retrieving the dry -> wet and wet -> wet probabilities
     * for the given location and month. We pattern match on the result of
     * List.map to avoid typing out the same function call twice. The pattern
     * match is non-exhaustive, but we can be sure that we'll only get as many
     * elements back from the map as we put into it, so this is safe. *)
    let [dry_wet; wet_wet] =
        ["drywet.txt" ; "wetwet.txt"]
        |> List.map (transition_probability longitude latitude month) in
   
    (* We are still forced to handle the case where dry_wet or wet_wet were
     * not able to be retrieved and instead resulted in Errors. If they
     * were successfully retrieved, we define an aux function to generate
     * a list representing a month's forecast. Otherwise we Error. *)
    match (dry_wet, wet_wet) with
    | Ok (dry_wet), Ok (wet_wet) -> (

        let rec aux days acc =  (* All we need to generate the forecast now is the amount of days left in the month*) 
            if days > 0 then    (* and a running list of generated days. As long as there are days left to generate, *)
                match acc with  (* we find out if this is the first day or not based on whether acc is empty. *)
                | [] ->         (* If it is, we generate the first day: 50% chance it'll be wet or dry. *)
                        let first_day = if Random.float 1.0 > 0.5 then Wet else Dry in
                        aux (days - 1) [first_day] (* Then we add it to the acc and call aux recursively with one less remaining day *)
                | h::_ ->   (* If acc isn't empty, *)
                    let today = gen_today dry_wet wet_wet h in (* we look at yesterday and use it to determine whether today will be Wet or Dry *)
                    aux (days - 1) (today :: acc) (* We then push today onto our acc and call aux with one less remaining day *)
            else
                acc (* If we're out of days, we just return acc *)
        in
        (* We reverse the result of calling aux on the amount of days in the given month, 
         * then we wrap it in Ok. *)
        Ok ((aux (days_in_month.(month - 2)) []) |> List.rev) 
        )
    | _ -> Error "Unable to determine probabilities from input files." (* Of course, if there was an error getting our probabilities, we error. *)


let longest_spell weather forecast =
    (* We define a recursive aux function that keeps track
     * of the current max as well as current total of consecutive
     * days of the given weather. If a day whose weather does
     * not match whats given is encountered, the acc is reset
     * and depending on whether it was greater than the current
     * max, the current max may be updated. *)
    let rec aux max acc l = 
        match l with
        | [] -> max
        | h::t ->
                if h = weather then
                    aux max (acc + 1) t
                else
                    if acc > max then
                        aux acc 0 t
                    else
                        aux max 0 t
    in
    aux 0 0 forecast

(* Simple function for counting the number of days
 * in a forecast with a certain type of weather *)
let number_of weather forecast =
    forecast
    |> List.filter ((=) weather)
    |> List.length


let () =
    let forecast = one_month_generator Sys.argv.(1) Sys.argv.(2) (int_of_string Sys.argv.(3)) in
    match forecast with
    | Ok forecast -> 
            forecast
            |> List.iter print_weather;
            
            forecast
            |> number_of Wet
            |> Printf.printf "%d\n";
            
            forecast
            |> longest_spell Wet
            |> Printf.printf "%d\n";
    | Error s -> print_endline s
