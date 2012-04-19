let f1 o1 o2 = match o1, o2 with
  | true, true->true
  | false, true|true, false->false
  | false, false->true

let f2 o1 o2 = ((o1 & o2) or ((not o1) & (not o2)))

let f3 (o1: bool) o2 = o1 = o2

let f4 (o1: bool) o2 = o1 == o2

let f5 o1 o2 = (o1 && o2) || ((not o1) && (not o2))

let f6 (o1:bool) o2 = not (o1 <> o2)

let f7 (o1: int) o2 = o1 = o2

let run1 n =
  for i = 1 to n do
    ignore(f1 true true);
    ignore(f1 true false);
    ignore(f1 false true);
    ignore(f1 false false);
  done

let run2 n =
  for i = 1 to n do
    ignore(f2 true true);
    ignore(f2 true false);
    ignore(f2 false true);
    ignore(f2 false false);
  done

let run3 n =
  for i = 1 to n do
    ignore(f3 true true);
    ignore(f3 true false);
    ignore(f3 false true);
    ignore(f3 false false);
  done

let run4 n =
  for i = 1 to n do
    ignore(f4 true true);
    ignore(f4 true false);
    ignore(f4 false true);
    ignore(f4 false false);
  done

let run5 n =
  for i = 1 to n do
    ignore(f5 true true);
    ignore(f5 true false);
    ignore(f5 false true);
    ignore(f5 false false);
  done

let run6 n =
  for i = 1 to n do
    ignore(f6 true true);
    ignore(f6 true false);
    ignore(f6 false true);
    ignore(f6 false false);
  done

let run7 n =
  for i = 1 to n do
    ignore(f7 1 1);
    ignore(f7 1 0);
    ignore(f7 0 1);
    ignore(f7 0 0);
  done

let () =
  Bench.config.Bench.samples <- 10_000;
  Bench.summarize (Bench.bench_n ["f1", run1; "f2", run2; "f3", run3; "f4", run4; "f5", run5; "f6", run6; "f7", run7])
