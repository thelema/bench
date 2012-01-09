open Printf

let r0 = [|
3.20937_75891_38469_47256_2e+03, 2.84423_68334_39170_62227_3e+03;
3.77485_23768_53020_20813_7e+02, 1.28261_65260_77372_27564_5e+03;
1.13864_15415_10501_55649_5e+02, 2.44024_63793_44441_73305_6e+02;
3.16112_37438_70565_59694_7e+00, 2.36012_90952_34412_09349_9e+01;
1.85777_70618_46031_52673_0e-01, 1.00000_00000_00000_00000_0e+00;
|]

let r1 = [|
1.23033_93547_97997_25272e+03, 1.23033_93548_03749_42043e+03;
2.05107_83778_26071_46532e+03, 3.43936_76741_43721_63696e+03;
1.71204_76126_34070_58314e+03, 4.36261_90901_43247_15820e+03;
8.81952_22124_17690_90411e+02, 3.29079_92357_33459_62678e+03;
2.98635_13819_74001_31132e+02, 1.62138_95745_66690_18874e+03;
6.61191_90637_14162_94775e+01, 5.37181_10186_20098_57509e+02;
8.88314_97943_88375_94118e+00, 1.17693_95089_13124_99305e+02;
5.64188_49698_86700_89180e-01, 1.57449_26110_70983_47253e+01;
2.15311_53547_44038_46343e-08, 1.00000_00000_00000_00000e+00;
|]

let r2 = [|
-6.58749_16152_98378_03157e-04, 2.33520_49762_68691_85443e-03;
-1.60837_85148_74227_66278e-02, 6.05183_41312_44131_91178e-02;
-1.25781_72611_12292_46204e-01, 5.27905_10295_14284_12248e-01;
-3.60344_89994_98044_39429e-01, 1.87295_28499_23460_47209e+00;
-3.05326_63496_12323_44035e-01, 2.56852_01922_89822_42072e+00;
-1.63153_87137_30209_78498e-02, 1.00000_00000_00000_00000e+00;
|]


let horner2 r x =
  let n = Array.length r in
  let s = ref 0.
  and t = ref 0. in
  for i = n - 1 downto 0 do
    let p, q = Array.unsafe_get r i in
    s := !s *. x +. p;
    t := !t *. x +. q
  done;
  !s /. !t


let iqpi = 5.64189_58354_77562_86948_1e-01

let erfc x =
  let z  = abs_float x in
  let z2 = z *. z in
  let y =
    if z < 0.46875 then   1. -.         z   *. horner2 r0 z2 else
    if z < 4.      then         exp (-. z2) *. horner2 r1 z  else
    let z'  = 1. /. z  in
    let z'2 = z' *. z' in       exp (-. z2) *. z' *. (iqpi +. z'2 *. horner2 r2 z'2)
  in if x < 0. then 2. -. y else y

let erf x = 1. -. erfc x


(* Incomplete gamma function
   1 / Gamma(a) * Int_0^x exp(-t) t^(a-1) dt *)
let rec p_gamma a x loggamma_a =
  if x >= 1. +. a then 1. -. q_gamma a x loggamma_a
  else if x = 0. then 0.
  else
    let rec pg_loop prev res term k =
      if k > 1000. then (eprintf "p_gamma could not converge."; res)
      else if prev = res then res
      else
        let term = term *. x /. (a +. k) in
        pg_loop res (res +. term) term (k +. 1.)
    in
    let r0 =  exp (a *. log x -. x -. loggamma_a) /. a in
    pg_loop min_float r0 r0 1.

(* Incomplete gamma function
   1 / Gamma(a) * Int_x^inf exp(-t) t^(a-1) dt  *)
and q_gamma a x loggamma_a =
  if x < 1. +. a then 1. -. p_gamma a x loggamma_a
  else
    let rec qg_loop prev res la lb w k =
      if k > 1000. then (eprintf "q_gamma could not converge."; res)
      else if prev = res then res
      else
        let la, lb =
          lb, ((k -. 1. -. a) *. (lb -. la) +. (k +. x) *. lb) /. k
        in
        let w = w *. (k -. 1. -. a) /. k in
        let prev, res = res, res +. w /. (la *. lb) in
        qg_loop prev res la lb w (k +. 1.)
    in
    let w = exp (a *. log x -. x -. loggamma_a) in
    let lb = (1. +. x -. a) in
    qg_loop min_float (w /. lb) 1. lb w 2.0

let pi = 4. *. atan 1.
let sqrt_2 = sqrt 2.0
let sqrt_2_pi = pi *. sqrt_2
let log_pi_over_2 = 0.572364942924700087071713675675 (*log_e(pi)/2*)

let erf2 = function
  | x when classify_float x = FP_nan -> x
  | x when classify_float x = FP_infinite -> if x > 0. then 1. else -1.
  | x when x > 0. -> p_gamma 0.5 (x *. x) log_pi_over_2
  | x (* x < 0 *) -> -. p_gamma 0.5 (x *. x) log_pi_over_2
let erfc2 = function
  | x when classify_float x = FP_nan -> x
  | x when classify_float x = FP_infinite -> if x > 0. then 0. else 2.
  | x when x >= 0. -> q_gamma 0.5 (x *. x) log_pi_over_2
  | x (* x < 0. *) -> 1. +. p_gamma 0.5 (x *. x) log_pi_over_2



let n = 100
let points = Array.init n (fun _ -> Random.float 10. -. 5.)

let test f () = for i = 0 to n-1 do ignore (f points.(i)); done

let () = Bench.bench ["erf", test erf;
                      "erf2", test erf2;]
