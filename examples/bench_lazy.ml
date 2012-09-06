(* cd .. && ocamlbuild examples/bench_lazy.native && ./bench_lazy.native *)

open Bench
open Printf



let x1 = lazy (20 * 30)
let ro = ref (-1)
let x2 = fun () -> if !ro = (-1) then ro := (20 * 30); !ro

let late1 () = Lazy.force x1
let late2 () = x2 ()


let () =
  printf "Lazy tag: %d\n%!" (Obj.tag (Obj.repr x1));
  Bench.bench ["lazy", late1; "closure", late2];
  printf "Lazy tag: %d\n%!" (Obj.tag (Obj.repr x1));
  Gc.compact();
  printf "Lazy tag: %d\n%!" (Obj.tag (Obj.repr x1));
  Bench.bench ["lazy", late1; "closure", late2];
  printf "Lazy tag: %d\n%!" (Obj.tag (Obj.repr x1));
  ()


(*(summarize ~alpha:0.05 (bench ["mid1", harn mid1; "mid2", harn mid2; "mid3", harn mid3; "mid4", harn mid4; "mid5", harn mid5; "mid6", harn mid6])) *)
