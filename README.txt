(* OASIS_START *)
(* DO NOT EDIT (digest: 5efd74c1b5f2d5b5099d04190beba25c) *)
This is the README file for the bench distribution.

A benchmarking tool for statistically valid benchmarks

See the files INSTALL.txt for building and installation instructions. 


(* OASIS_STOP *)

To compile example programs, use ocamlbuild like this:
   
    ocamlbuild example/test_int.native

This will produce [test_int.native] in the current directory, which
can be executed directly to run those benchmarks.
