(* cd .. && ocamlbuild benchsuite/test_rand.native && _build/benchsuite/test_rand.native *)

open Bench

module R311 = struct

  type t = { st : int array; mutable idx : int };;

  let new_state () = { st = Array.make 55 0; idx = 0 };;
  let assign st1 st2 =
    Array.blit st2.st 0 st1.st 0 55;
    st1.idx <- st2.idx;
  ;;

  let full_init s seed =
    let combine accu x = Digest.string (accu ^ string_of_int x) in
    let extract d =
      (Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16))
      lxor (Char.code d.[3] lsl 22)
    in
    let l = Array.length seed in
    for i = 0 to 54 do
      s.st.(i) <- i;
    done;
    let accu = ref "x" in
    for i = 0 to 54 + max 55 l do
      let j = i mod 55 in
      let k = i mod l in
      accu := combine !accu seed.(k);
      s.st.(j) <- s.st.(j) lxor extract !accu;
    done;
    s.idx <- 0;
  ;;

  let make seed =
    let result = new_state () in
    full_init result seed;
    result
  ;;

  let copy s =
    let result = new_state () in
    assign result s;
    result
  ;;

  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let bits s =
    s.idx <- (s.idx + 1) mod 55;
    let newval = (s.st.((s.idx + 24) mod 55) + s.st.(s.idx)) land 0x3FFFFFFF in
    s.st.(s.idx) <- newval;
    newval
  ;;

  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v
  ;;
  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound
  ;;

  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v
  ;;
  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound
  ;;

  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v
  ;;
  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound
  ;;

  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))
  ;;

  (* Returns a float 0 <= x < 1 with at most 90 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0
    and r0 = Pervasives.float (bits s)
    and r1 = Pervasives.float (bits s)
    and r2 = Pervasives.float (bits s)
    in ((r0 /. scale +. r1) /. scale +. r2) /. scale
  ;;

  let float s bound = rawfloat s *. bound;;

  let bool s = (bits s land 1 = 0);;

let default = {
  st = [|
      509760043; 399328820; 99941072; 112282318; 611886020; 516451399;
      626288598; 337482183; 748548471; 808894867; 657927153; 386437385;
      42355480; 977713532; 311548488; 13857891; 307938721; 93724463;
      1041159001; 444711218; 1040610926; 233671814; 664494626; 1071756703;
      188709089; 420289414; 969883075; 513442196; 275039308; 918830973;
      598627151; 134083417; 823987070; 619204222; 81893604; 871834315;
      398384680; 475117924; 520153386; 324637501; 38588599; 435158812;
      168033706; 585877294; 328347186; 293179100; 671391820; 846150845;
      283985689; 502873302; 718642511; 938465128; 962756406; 107944131;
      192910970;
    |];
  idx = 0;
}
let init s = full_init default [|s|]
end;;

module R312 = struct

  type t = { st : int array; mutable idx : int };;

  let new_state () = { st = Array.make 55 0; idx = 0 };;
  let assign st1 st2 =
    Array.blit st2.st 0 st1.st 0 55;
    st1.idx <- st2.idx;
  ;;

  let full_init s seed =
    let combine accu x = Digest.string (accu ^ string_of_int x) in
    let extract d =
      Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16)
      + (Char.code d.[3] lsl 24)
    in
    let seed = if seed = [| |] then [| 0 |] else seed in
    let l = Array.length seed in
    for i = 0 to 54 do
      s.st.(i) <- i;
    done;
    let accu = ref "x" in
    for i = 0 to 54 + max 55 l do
      let j = i mod 55 in
      let k = i mod l in
      accu := combine !accu seed.(k);
      s.st.(j) <- s.st.(j) lxor extract !accu;
    done;
    s.idx <- 0;
  ;;

  let make seed =
    let result = new_state () in
    full_init result seed;
    result
  ;;

  let copy s =
    let result = new_state () in
    assign result s;
    result
  ;;

  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let bits s =
    s.idx <- (s.idx + 1) mod 55;
    let newval = s.st.((s.idx + 24) mod 55)
                 + (s.st.(s.idx) lxor ((s.st.(s.idx) lsr 25) land 31)) in
    s.st.(s.idx) <- newval;
    newval land 0x3FFFFFFF   (* land is needed for 64-bit arch *)
  ;;

  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v
  ;;
  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound
  ;;

  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v
  ;;
  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound
  ;;

  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v
  ;;
  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound
  ;;

  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))
  ;;

  (* Returns a float 0 <= x < 1 with at most 90 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0
    and r0 = Pervasives.float (bits s)
    and r1 = Pervasives.float (bits s)
    and r2 = Pervasives.float (bits s)
    in ((r0 /. scale +. r1) /. scale +. r2) /. scale
  ;;

  let float s bound = rawfloat s *. bound;;

  let bool s = (bits s land 1 = 0);;
(* This is the state you get with [init 27182818]. *)
let default = {
  st = [|
      0x7ae2522b; 0x5d8d4634; 0x15b4fad0; 0x18b14ace; 0x12f8a3c4; 0x7b086c47;
      0x16d467d6; 0x501d91c7; 0x321df177; 0x4176c193; 0x1ff72bf1; 0x5e889109;
      0x0b464b18; 0x6b86b97c; 0x4891da48; 0x03137463; 0x485ac5a1; 0x15d61f2f;
      0x7bced359; 0x69c1c132; 0x7a86766e; 0x366d8c86; 0x1f5b6222; 0x7ce1b59f;
      0x2ebf78e1; 0x67cd1b86; 0x658f3dc3; 0x789a8194; 0x42e4c44c; 0x58c43f7d;
      0x0f6e534f; 0x1e7df359; 0x455d0b7e; 0x10e84e7e; 0x126198e4; 0x4e7722cb;
      0x5cbede28; 0x7391b964; 0x7d40e92a; 0x4c59933d; 0x0b8cd0b7; 0x64efff1c;
      0x2803fdaa; 0x08ebc72e; 0x4f522e32; 0x45398edc; 0x2144a04c; 0x4aef3cbd;
      0x41ad4719; 0x75b93cd6; 0x2a559d4f; 0x5e6fd768; 0x66e27f36; 0x186f18c3;
      0x2fbf967a;
    |];
  idx = 0;
};;
let init s = full_init default [|s|]
end;;

module Mersenne = struct
(* Mersenne Twister PRNG for ocaml
   Copyright (C) 2003 Shawn Wagner <raevnos@pennmush.org>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
	 
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
	 
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* Ocaml version of the Mersenne Twister random number generator based
   on mt19937ar.c.

   http://www.math.keio.ac.jp/matumoto/emt.html

*)

(* Period parameters *)
let n = 624
let m = 397

type t = {
  mt: int32 array;
  mutable mti: int;
}


(* Tempering parameters *)
(* Ocaml really needs a way to write literal int32's and int64s! *)
let mask_b = 0x9d2c5680l
and mask_c = 0xefc60000l
and upper_mask = 0x80000000l (* most significant w-r bits *)
and lower_mask = 0x7fffffffl (* least significant r bits *)
and mag01 = [| 0l; 0x9908b0dfl |] 
and mask_high = 0xffff0000l

let new_state () =
  {
    mt = Array.make n 0l;
    mti = n + 1;
  }

let init32 seed = 
  let t = new_state () in
  let m1 = 1812433253l
  and mfill = 0xffffffffl in
    Array.unsafe_set t.mt 0 (Int32.logand seed mfill);
    for i = 1 to n - 1 do
      let mtprev = Array.unsafe_get t.mt (i - 1) in
        Array.unsafe_set t.mt i (Int32.add
                           (Int32.mul m1
                              (Int32.logxor mtprev
                                 (Int32.shift_right_logical mtprev 30)))
                              (Int32.of_int i))
    done;
    t

let init_array32 seed =
  let t = init32 19650218l in
  let k = ref (max n (Array.length seed))
  and i = ref 1 
  and j = ref 0 
  and mval1 = 1664525l
  and mval2 = 1566083941l
  in
    while !k > 0 do
      let mtprev = Array.unsafe_get t.mt (!i - 1) in
        Array.unsafe_set t.mt !i
          (Int32.add
             (Int32.add
                (Int32.logxor (Array.unsafe_get t.mt !i)
                   (Int32.mul
                      (Int32.logxor mtprev
                         (Int32.shift_right_logical mtprev 30))
                      mval1))
                seed.(!j))
             (Int32.of_int !j));
        incr i;
        incr j;
        if !i >= n then begin
          Array.unsafe_set t.mt 0 (Array.unsafe_get t.mt (n - 1));
          i := 1
        end;
        if !j >= Array.length seed then j := 0;
        decr k
    done;

    k := n - 1;

    while !k > 0 do
      let mtprev = Array.unsafe_get t.mt (!i - 1) in
        Array.unsafe_set t.mt !i
          (Int32.sub
           (Int32.logxor
              (Array.unsafe_get t.mt !i)
              (Int32.mul
                 (Int32.logxor
                    mtprev
                    (Int32.shift_right_logical mtprev 30))
                    mval2))
              (Int32.of_int !i));
        incr i;
        if !i >= n then begin
          Array.unsafe_set t.mt 0 (Array.unsafe_get t.mt (n - 1));
          i := 1
        end;
        decr k
    done;
    Array.unsafe_set t.mt 0 0x80000000l;
    t

let make = function
  | `Seed32 x -> init32 x
  | `Seed x -> init32 (Int32.of_int x)
  | `Array32 x -> init_array32 x
  | `Array x -> init_array32 (Array.map Int32.of_int x)
  | `CurrentTime -> init32 (Int32.of_float (Sys.time ()))

      
let fill_mt t =
  let y = ref 0l
  and kk = ref 0 in 
  let fiddle i ip ig =
    y := Int32.logor
      (Int32.logand (Array.unsafe_get t.mt i) upper_mask)
      (Int32.logand (Array.unsafe_get t.mt ip) lower_mask);
    Array.unsafe_set t.mt i
      (Int32.logxor
         (Int32.logxor
            (Array.unsafe_get t.mt ig)
            (Int32.shift_right_logical !y 1))
         (Array.unsafe_get mag01 (Int32.to_int (Int32.logand !y Int32.one))))
  in
    while !kk < n - m do
      fiddle !kk (!kk + 1) (!kk + m);
      incr kk
    done;
    while !kk < n - 1 do
      fiddle !kk (!kk + 1) (!kk + m - n);
      incr kk
    done;
    fiddle (n - 1) 0 (m - 1);
    t.mti <- 0

let uint32 t =
  if t.mti >= n then fill_mt t;
  let y = Array.unsafe_get t.mt t.mti in
  let y' = Int32.logxor y (Int32.shift_right_logical y 11) in
  let y = Int32.logxor y' (Int32.logand (Int32.shift_left y' 7)
                             mask_b) in
  let y' = Int32.logxor y (Int32.logand (Int32.shift_left y 15)
                             mask_c) in
  let r = Int32.logxor y' (Int32.shift_right_logical y' 18) in
    t.mti <- t.mti + 1;
    r
     
let int32 t =
  let r = uint32 t in
    Int32.shift_right_logical r 1
 
let uint64 t =
  let high = Int64.of_int32 (uint32 t)
  and low = Int64.of_int32 (uint32 t) in
    Int64.logor low (Int64.shift_left high 32) 

let int64 t =
  let r = uint64 t in
    Int64.shift_right_logical r 1

let unativeint t =
  let v = Nativeint.of_int32 (uint32 t) in
    if Sys.word_size = 32 then v
  else Nativeint.logor v
    (Nativeint.shift_left (Nativeint.of_int32 (uint32 t)) 32)
  
let nativeint t =
  let r = unativeint t in
    Nativeint.shift_right_logical r 1

let uint t =
  let v = unativeint t in
    Nativeint.to_int (Nativeint.shift_right_logical v 1)

let int t =
  let r = unativeint t in
    Nativeint.to_int (Nativeint.shift_right_logical r 2)

let uint32_to_float ui32 =
  if ui32 >= Int32.zero then
    Int32.to_float ui32
  else (* ACK! *) 
    float_of_string (Printf.sprintf "%lu" ui32)

let real0 t =
  let i32 = uint32 t in
    uint32_to_float i32

let real1 t =
  let r = real0 t in
    r *. (1.0 /. 4294967295.0)

let real2 t =
  let r= real0 t in
    r *. (1.0 /. 4294967296.0)

let real3 t =
  let r = real0 t in
    (r +. 0.5) *. (1.0 /. 4294967296.0)

let res53 t =
  let a = Int32.shift_right_logical (uint32 t) 5 in
  let b = Int32.shift_right_logical (uint32 t) 6 in
  let a' = uint32_to_float a
  and b' = uint32_to_float b in
    (a' *. 67108864.0 +. b') *. (1.0 /. 9007199254740992.0)

end


let test_rand_bench () = 
  let rand311 n = 
    R311.init 27182818;
    for i = 1 to n do
      ignore (R311.float R311.default 1.0);
    done;    
  in

  let rand312 n = 
    R312.init 27182818;
    for i = 1 to n do
      ignore (R312.float R312.default 1.0);
    done;
  in

  let mt n = 
    let m = Mersenne.make (`Seed 27182818) in
    for i = 1 to n do
      ignore(Mersenne.real1 m);
    done;
  in

  let tests = 
    ["OCaml 3.12 Random", rand312;
     "OCaml 3.11 Random", rand311;
     "Mersenne Twister", mt;
    ]
  in
  bench_n tests

let () =
  test_rand_bench ()
