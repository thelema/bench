(** Funny benchmark of various ways to do the same thing, motivated by
    Martin DeMello's caml-list question at
    https://sympa-roc.inria.fr/wws/arc/caml-list/2012-01/msg00165.html *)

let o = 1 and v = 2

let demello = function
  | (true, true)  -> [o; v]
  | (false, true) -> [v]
  | (true, false) -> [o]
  | (false, false) -> []

let robert (out, value) =
  (if out then [o] else []) @ (if value then [v] else [])

let b2l b x = if b then [x] else [];;

let ferre (out, value) =
  b2l out o @
    b2l value v;;

let maybe b v c = if b then v :: c else c
let vuillion (out, value) = maybe out o (maybe value v [])

let (|>) x f = f x (* no %revapply yet *)
let lefessant (out, value) = [] |> maybe value v |> maybe out o

let lin (out, value) = List.filter (fun x -> x)  [out; value]
let lin2 (out, value) = List.map snd (List.filter (fun x -> fst x) [(out, o); (value, v)])

let friendly (out,value) =
  if out then if value then [o;v] else [o]
  else if value then [v] else []

let test f n =
  for _ = 1 to n do
    ignore(f (true, true));
    ignore(f (true, false));
    ignore(f (false, true));
    ignore(f (false, false));
  done

let () = Bench.bench_n [
  "demello", test demello;
  "robert", test robert;
  "ferre", test ferre;
  "vuillion", test vuillion;
  "lefessant", test lefessant;
  "lin", test lin;
  "lin2", test lin2;
  "friendly", test friendly;
] |> Bench.print_1d "match.1d"
