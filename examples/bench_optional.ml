open Bench

let round1 ?(p=1.0) x = p *. (floor ((x /. p) +. 0.5))
let round2 = function
  | None -> fun x -> 1.0 *. (floor ((x /. 1.0) +. 0.5))
  | Some p -> fun x -> p *. (floor ((x /. p) +. 0.5))

let floats = Array.init 1000 (fun _ -> Random.float 20.0)

let f1 n =
  let round = round1 ~p:10. in
  for i = 1 to n do
    Array.iter (fun x -> ignore(round x)) floats
  done

let f2 n =
  let round = round2 (Some 10.) in
  for i = 1 to n do
    Array.iter (fun x -> ignore(round x)) floats
  done

let f3 n =
  let round x = 10. *. (floor ((x /. 10.) +. 0.5)) in
  for i = 1 to n do
    Array.iter (fun x -> ignore(round x)) floats
  done


let () = Bench.run_outputs (Bench.bench_n ["opt", f1; "hand", f2; "fixed", f3])
