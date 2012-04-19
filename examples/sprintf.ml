let () =
  Bench.bench ["smake", (fun () -> String.make 1 'x');
               "sprintf", (fun () -> Printf.sprintf "%c" 'x')]
