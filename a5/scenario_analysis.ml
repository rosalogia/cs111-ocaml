open Vehicle
open Fuel
open Lease
open Str


type context =
    { gas_price: float
    ; electricity_price: float
    ; vehicles: Vehicle.t list }

let read_vehicles vehicle_file =
    let open Scanf in
    let ic = Scanning.open_in vehicle_file in

    let single in_channel =
        let id a = a in
        let get_value fstring = bscanf in_channel fstring id in

        let fuel_type           = get_value "%s " in
        let name                = get_value "%s " in
        let due_at_signing      = get_value "%f " in
        let number_of_months    = get_value "%d " in
        let monthly_cost        = get_value "%f " in
        let mileage_allowance   = get_value "%d " in
        let usage               = get_value "%f " in
        let kwh_per_charge      = if fuel_type = "gas" then None else Some (get_value "%f ") in
        let other_cost          = get_value "%f " in
        
        let fuel: Fuel.t =
            match kwh_per_charge with
            | Some (kpc)    -> Electric (usage, kpc)
            | None          -> Gas (usage) in

        let lease: Lease.t =
            { due_at_signing    = due_at_signing
            ; number_of_months  = number_of_months
            ; monthly_cost      = monthly_cost
            ; mileage_allowance = mileage_allowance } in
        
        let vehicle: Vehicle.t =
            { name          = name
            ; fuel          = fuel
            ; lease         = lease
            ; co2_emissions = 0.0
            ; other_cost    = other_cost
            ; fuel_cost     = 0.0
            ; total_cost    = 0.0 } in
        vehicle in

    let rec aux acc in_channel =
        try
            aux (acc @ [single in_channel]) in_channel
        with End_of_file ->
            acc in
    aux [] ic

let context_with gas_price electricity_price vehicle_file =
    let vehicles = read_vehicles vehicle_file in
    { gas_price         = gas_price
    ; electricity_price = electricity_price
    ; vehicles          = vehicles }


let compute_co2ec context =
    let compute_single (vehicle: Vehicle.t) =
        let (lease, fuel, months) = (vehicle.lease, vehicle.fuel, vehicle.lease.number_of_months) in
        let mileage = (float_of_int months /. 12.0) *. (float_of_int lease.mileage_allowance) in

        let (usage, ec, cc) =
            match fuel with
            | Gas (usage) ->
                    (usage, 1.0, context.gas_price)
            | Electric (usage, kwh_per_charge) ->
                    let e = Fuel.co2_emitted_generate_mwh *. 0.45 /. 1000.0 in
                    (usage, kwh_per_charge, context.electricity_price *. e /. 100.0) in

        let fc = cc *. mileage /. usage in
        let emissions = mileage /. usage *. ec in

        { vehicle with  fuel_cost = fc;
                        co2_emissions = emissions;
                        total_cost = fc +. lease.due_at_signing +. lease.monthly_cost *. (float_of_int months) +. vehicle.other_cost }
        in

    { context with  vehicles =
                        context.vehicles
                        |> List.map compute_single }

let () =
    context_with 2.23 16.14 "vehicles.txt"
    |> compute_co2ec
    |> (fun ctx -> ctx.vehicles)
    |> List.map (Vehicle.string_of)
    |> List.iter print_endline

