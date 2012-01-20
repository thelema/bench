(*
 * Bench - Benchmarking functions
 * Copyright (C) 2011 Edgar Friendly
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Benchmarking toolbox, based on the Criterion library by Bryan
    O'Sullivan

    @author Edgar Friendly <thelema314\@gmail.com>
 *)

(** {6 Easy Benchmarking} *)

(** The following function suffices for most uses of this library.  To
    benchmark the (unit -> unit) functions [f],[g],and [h], the
    following will run the benchmark and print a comparison: {[
    Bench.bench ["f",f; "g",g; "h",h] ]} The number of iterations to
    run per sample and number of samples to take will be computed
    automatically.  *)

(** Benchmark many unit functions. *)
val bench : (string * (unit -> 'a)) list -> unit

(** {6 Internal result format} *)

module Bootstrap : sig
  type estimate = {
    point : float;
    lower : float;
    upper : float;
    confidence : float;
  }
end

(** The results of running a test *)
type results = {
  (** the string description of that test *)
  desc : string;

  (** The measured times for each sample *)
  times : float array;

  (** The mean time with bootstrapped CI *)
  mean : Bootstrap.estimate;

  (** The stdev of time with bootstrapped CI *)
  stdev : Bootstrap.estimate;

  (** How much the outliers affected the variance, as a percentage.
      0% means no effect, 100% means the outliers were responsible for all
      the variance. *)
  ov : float;
}

(** {6 Detailed benchmarking functions} *)

(** [bench fs] benchmarks the named functions with argument given in
    [fs]
    {[
    let res = Bench.bench_arg ["div", ( /. ) 3., 4.;
                              "mul", ( *. ) 3., 0.25] in
    Bench.run_outputs res
    ]}
 *)
val bench_arg : (string * ('a -> 'b) * 'a) list -> results list

(** Benchmark one function with many arguments.
    {[
    let () = Bench.run_outputs (Bench.bench_args List.rev [
        "10", Array.to_list (Array.create 10 0);
        "100", Array.to_list (Array.create 100 0);
        "1000", Array.to_list (Array.create 1000 0);
        ])
    ]}
*)
val bench_args : ('a -> 'b) -> (string * 'a) list -> results list

(** Benchmark many functions with a fixed parameter.
    let l = Array.to_list (Array.init 1000 (fun _ -> Random.float 20.)) in
    let res = Bench.bench_funs [
                  "listsort", List.sort compare;
                  "arrsort", (fun l -> let a = Array.of_list l in Array.sort compare a; Array.to_list a);
                  ] l in
    Bench.run_outputs res

*)
val bench_funs : (string * ('a -> 'b)) list -> 'a -> results list

(** This function benchmarks functions that take a single parameter of
    how many repetitions to run.  This is slightly more accurate
    benchmark than others, as the function isn't wrapped by an
    extension function that calls it many times.  *)
val bench_n : (string * (int -> 'a)) list -> results list

(** Benchmark a function for throughput.  The function's argument is
    the block size it will work over, and the block sizes to test are
    given in a list.  The results are in terms of throughput - the
    time taken divided by the size of the block.  *)
val bench_throughput : (int -> 'a) -> int list -> results list

val bench_range : ('a -> 'b) -> input_gen:(int -> 'a) -> ?n:int -> (int * int) -> results list

val bench_2d : (string * ('a -> 'b)) list -> input_gen:(int -> 'a) -> ?n:int -> (int * int) -> (int list * (string * results list) list)

val print_2d : string -> (int list * (string * results list) list) -> unit
val print_1d : string -> results list -> unit

(** The function that summarizes the results by comparing the results
    linearly.  The first parameter is the alpha (type I error rate)
    for the "same time" test, the second is the list of results from
    the tests. *)
val summarize : ?alpha:float -> results list -> unit

(** {6 Configuration API} *)

(** The following global configuration parameters are available for
    bench *)
type config = {
  mutable verbose : bool;
  (** bench will print much more progress information if this is true.
      Default=true. *)

  mutable samples : int;
  (** The minimum number of samples to measure Default=1000 *)

  mutable gc_between_tests : bool;
  (** Whether or not to call [Gc.compact] between tests
      Default=false*)

  mutable resamples : int;
  (** The number of resamples to use when computing confidence
      intervals Default=1000 *)

  mutable confidence_interval : float;
  (** How big a confidence interval to estimate for mean and stdev Default: 95% (0.95)*)

  mutable output : (results list -> unit) list;
  (** Output functions to use.  Default: [summarize 0.05] *)

}

(* This module's global config *)
val config : config

(** Initialize the benchmark environment by measuring the clock
   resolution and cost.  This function is called automatically if
   these measurements are needed, and calling it a second time does
   nothing. *)
val init_environment : unit -> unit
