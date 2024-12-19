let registry =
  Registry.empty ()
  |> Registry.add ~year:2015 ~day:1 ~part:1 ~value:Year_2015.Day_01.part_one
  |> Registry.add ~year:2015 ~day:1 ~part:2 ~value:Year_2015.Day_01.part_two
  |> Registry.add ~year:2015 ~day:2 ~part:1 ~value:Year_2015.Day_02.part_one
  |> Registry.add ~year:2015 ~day:2 ~part:2 ~value:Year_2015.Day_02.part_two
  |> Registry.add ~year:2015 ~day:3 ~part:1 ~value:Year_2015.Day_03.part_one
  |> Registry.add ~year:2015 ~day:3 ~part:2 ~value:Year_2015.Day_03.part_two
  |> Registry.add ~year:2015 ~day:4 ~part:1 ~value:Year_2015.Day_04.part_one
  |> Registry.add ~year:2015 ~day:4 ~part:2 ~value:Year_2015.Day_04.part_two
  |> Registry.add ~year:2015 ~day:5 ~part:1 ~value:Year_2015.Day_05.part_one
  |> Registry.add ~year:2015 ~day:5 ~part:2 ~value:Year_2015.Day_05.part_two
  |> Registry.add ~year:2024 ~day:1 ~part:1 ~value:Year_2024.Day_01.part_one
  |> Registry.add ~year:2024 ~day:1 ~part:2 ~value:Year_2024.Day_01.part_two
  |> Registry.add ~year:2024 ~day:2 ~part:1 ~value:Year_2024.Day_02.part_one
  |> Registry.add ~year:2024 ~day:2 ~part:2 ~value:Year_2024.Day_02.part_two
  |> Registry.add ~year:2024 ~day:3 ~part:1 ~value:Year_2024.Day_03.part_one
  |> Registry.add ~year:2024 ~day:3 ~part:2 ~value:Year_2024.Day_03.part_two
  |> Registry.add ~year:2024 ~day:4 ~part:1 ~value:Year_2024.Day_04.part_one
  |> Registry.add ~year:2024 ~day:4 ~part:2 ~value:Year_2024.Day_04.part_two
  |> Registry.add ~year:2024 ~day:5 ~part:1 ~value:Year_2024.Day_05.part_one
  |> Registry.add ~year:2024 ~day:5 ~part:2 ~value:Year_2024.Day_05.part_two
  |> Registry.add ~year:2024 ~day:6 ~part:1 ~value:Year_2024.Day_06.part_one
  |> Registry.add ~year:2024 ~day:6 ~part:2 ~value:Year_2024.Day_06.part_two
  |> Registry.add ~year:2024 ~day:7 ~part:1 ~value:Year_2024.Day_07.part_one
  |> Registry.add ~year:2024 ~day:7 ~part:2 ~value:Year_2024.Day_07.part_two
  |> Registry.add ~year:2024 ~day:8 ~part:1 ~value:Year_2024.Day_08.part_one
  |> Registry.add ~year:2024 ~day:8 ~part:2 ~value:Year_2024.Day_08.part_two
;;

let years = Registry.all_years registry

let days ~year = Registry.all_days ~year registry

let parts ~year ~day = Registry.all_parts ~year ~day registry

exception No_solution

let solve ~year ~day ~part input =
  let solve = Registry.find_opt ~year ~day ~part registry in
  match solve with
  | Some solve -> input |> solve
  | None -> raise No_solution
;;
