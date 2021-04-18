type t =
    { due_at_signing: float
    ; number_of_months: int
    ; monthly_cost: float
    ; mileage_allowance: int }

let string_of lease =
    let open Printf in

    [
        "Lease" ;
        sprintf "\tDue at signing: %.2f" lease.due_at_signing ;
        sprintf "\tNumber of months: %d" lease.number_of_months ;
        sprintf "\tMonthly cost: %.2f" lease.monthly_cost ;
        sprintf "\tMileage allowance: %d" lease.mileage_allowance
    ] |> String.concat "\n"
