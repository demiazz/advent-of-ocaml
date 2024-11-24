let registry = Registry.empty ()
let years = Registry.all_years registry
let days ~year = Registry.all_days ~year registry
let parts ~year ~day = Registry.all_parts ~year ~day registry

exception No_solution

let solve ~year ~day ~part input =
  let solve = Registry.find_opt ~year ~day ~part registry in
  match solve with Some solve -> input |> solve | None -> raise No_solution
