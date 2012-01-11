open Batteries

let ident x = x
let () =
  Bench.bench_2d
    ["batlist.map", BatList.map ident;
     "stdlib map", List.map ident;
     "rev rev_map", (fun l -> List.rev (List.rev_map ident l))]
    ~input_gen:(fun n -> BatList.init n ident)
    (1,10_000)
  |> Bench.print_2d "lm.out"
