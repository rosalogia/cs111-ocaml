open Fuel
open Lease

type t =
    { name: string
    ; fuel: Fuel.t
    ; lease: Lease.t
    ; co2_emissions: float
    ; other_cost: float
    ; fuel_cost: float
    ; total_cost: float }

let string_of vehicle =
    let open Printf in

    [
        "\n____________________\n" ;
        sprintf "Vehicle %s" vehicle.name ;
        Fuel.string_of vehicle.fuel ;
        Lease.string_of vehicle.lease ;
        sprintf "\nCO2 Emission: %.2f kg/CO2\n" vehicle.co2_emissions ;
        "Cost" ;
        sprintf "\tOther: %2.f dollars" vehicle.other_cost ;
        sprintf "\tFuel: %.2f dollars for %d months of lease" vehicle.fuel_cost 1 ;
        sprintf "\tTotal: %.2f dollars for %d months of lease" vehicle.total_cost 1
    ] |> String.concat "\n"

