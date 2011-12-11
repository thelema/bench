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

(** {6 Benchmarking Wrappers} *)

(** The following functions suffice for most use of this library.  To
    benchmark the (unit -> unit) functions [f],[g],and [h], the
    following will run the benchmark and print a comparison: {[
    Bench.bench_unit ["f",f; "g",g; "h",h] ]} The number of iterations
    to run per sample and number of samples to take will be computed
    automatically.  *)

(** [bench fs] benchmarks the named functions with argument given in
    [fs] *)
val bench : (string * ('a -> unit) * 'a) list -> unit

(** Benchmark one function with many arguments. *)
val bench_args : ('a -> 'b) -> (string * 'a) list -> unit

(** Benchmark many unit functions. *)
val bench_unit : (string * (unit -> 'a)) list -> unit

(** Benchmark many functions with a fixed parameter. *)
val bench_funs : (string * ('a -> 'b)) list -> 'a -> unit

(** This function benchmarks functions that take a single parameter of
    how many repetitions to run.  This is slightly more accurate
    benchmark than others, as the function isn't wrapped by an
    extension function that calls it many times.  *)
val bench_n : (string * (int -> 'a)) list -> unit

(** Benchmark a function for throughput.  The function's argument is
    the block size it will work over, and the block sizes to test are
    given in a list.  The results are in terms of throughput - the
    time taken divided by the size of the block.  *)
val bench_throughput : (int -> 'a) -> int list -> unit

(** {6 Configuration API} *)

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

(** The function that summarizes the results by comparing the results
    linearly.  The first parameter is the alpha (type I error rate)
    for the "same time" test, the second is the list of results from
    the tests. *)
val summarize : float -> results list -> unit

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
      intervals Default=10000 *)

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

