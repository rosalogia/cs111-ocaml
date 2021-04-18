type t =
    | Gas of float
    | Electric of float * float

let string_of fuel =
    let open Printf in
    
    match fuel with
    | Gas (usage) ->
            sprintf "\nFuel\n\tType: Gas\n\tUsage: %.2f" usage
    | Electric (usage, kwh_per_charge) ->
            sprintf "\nFuel\n\tType: Electric\n\tUsage: %.2f\n\tkWhPerCharge: %.2f" usage kwh_per_charge

let co2_emitted_gas_combustion = 8.887
let co2_emitted_generate_mwh = 998.4

