open Unix

let filename = "sw_temp.tmp"

let gen_data buf bs = for i = 0 to bs-1 do buf.[i] <- Char.chr (i land 0xff); done in

let bench_buf bs = (* n is buffer size *)
  let buf = String.create bs in
  let fd = openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o700 in
  for i = 0 to 100 do
    gen_data buf bs;
    ignore (write fd buf 0 bs);
  done;
  close fd

let () =
  Bench.summarize (Bench.bench_throughput bench_buf [512; 1024; 2048; 4096; 8192; 16384; 32767; 65535])
