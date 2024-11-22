let registry =
  Registry.empty ()
  |> Registry.add ~year:2015 ~day:1 ~part:1 ~value:Year_2015.Day_01.part_one
  |> Registry.add ~year:2015 ~day:1 ~part:2 ~value:Year_2015.Day_01.part_two

let years = Registry.all_years registry
let days ~year = Registry.all_days ~year registry
let parts ~year ~day = Registry.all_parts ~year ~day registry

exception No_solution

let solve ~year ~day ~part input =
  let solve = Registry.find_opt ~year ~day ~part registry in
  match solve with Some solve -> input |> solve | None -> raise No_solution
