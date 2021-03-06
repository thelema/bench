
(* TODO
 * customize box
 *
 * Styles:  `Auto,`Bars,`Disks,`Impulses,`Lollipops
 *)

module A = Archimedes
module VP = A.Viewport
module AP = A.Path
module BE = A.Backend
module Clr = A.Color

let default_width  = ref 650
let default_height = ref 450

let color = ref A.Color.blue

let min_disk_width = ref 10
let max_disk_width = ref 20

let may f = function None -> () | Some v -> f v

let (|-) f g = fun x -> g (f x)
let (|>) x f = f x

let identity x = x

let range0 n = Array.init n identity

let fold f xs = Array.fold_left f xs.(0) (Array.sub xs 1 ((Array.length xs) - 1))

let ensure x f y = match x with Some x -> x | None -> f y

let for_all f xs =
  let num_xs = Array.length xs in
  let n = ref 0 in
  while (!n < num_xs && f xs.(!n)) do n := !n + 1 done;
  !n = num_xs

let rev_arr a =
  let l = Array.length a in
  Array.init l (fun i -> a.(l - i - 1))

type ydata = { fname: string; ests: float array; lows: float array; highs: float array }

let ylengths_equal n x = for_all ((=) n) (Array.map Array.length [|x.lows;x.ests;x.highs|])

let assert_y_order ydata =
  Array.iteri (fun n est -> assert (est > ydata.lows.(n) && est < ydata.highs.(n))) ydata.ests

let fold_fold f yss = fold f (Array.map (fold f) yss)

(* gap is between labels, height is height of label *)
let position_labels gap height pls =
  let compare_pl ((p1:float),_) (p2,_) = Pervasives.compare p1 p2 in
  let g_height ls = float (List.length ls - 1) *. (height +. gap) +. height in
  let does_overlap (pos1,ls1) (pos2,ls2) =
    let center_gap = abs_float (pos1 -. pos2) in
    let h1 = g_height ls1 and h2 = g_height ls2 in
    center_gap -. (h1/.2.) -. (h2/.2.) < gap
  in
  let merge_plgs (p1,ls1) (p2, ls2) =
    let l1 = float(List.length ls1) in
    let l2 = float(List.length ls2) in
    (p1 *. l1 +. p2 *. l2) /. (l1+.l2), ls1 @ ls2
  in
  let rec resolve_overlaps plgs =
    let rec find_overlap = function
      | plg1::plg2::tl when does_overlap plg1 plg2 ->
	find_overlap (merge_plgs plg1 plg2 :: tl)
      | h::(_::_ as tl) -> h :: find_overlap tl
      | ([_]|[]) as x -> x
    in
    let ovl = find_overlap plgs in
    if List.length ovl < List.length plgs
    then resolve_overlaps ovl (* repeat until no overlaps *)
    else ovl
  in
  let ungroup (p,ls) =
    let p0 = ref (p -. g_height ls /. 2. +. height /. 2.) in
    List.map (fun l -> let r = (!p0,l) in p0 := !p0 +. height +. gap; r) ls
  in
  let plgs = List.sort compare_pl (List.map (fun (pos, label) -> pos, [label]) pls) in
  let plgs_grouped = resolve_overlaps plgs in
  List.concat (List.map ungroup plgs_grouped)

let plot
    ?(outfile="plot_out.png")
    ?width:target_width
    ?(height = !default_height)
    ?(title="BatBench plot")
    ?(xlabel="Samples")
    ?(ylabel="Time")
    ?ymax
    ?(style=`Auto)
    ys =
  assert (!min_disk_width < !max_disk_width);
  let outfile = if Filename.check_suffix outfile ".png" then outfile else outfile ^ ".png" in
  let num_vals = Array.length ys in
  let (width,bar_width)  =
    match target_width with
    | Some w ->
	if w >= num_vals then (w, max 1 (w/num_vals))
	else invalid_arg "Insufficient width for data."
    | None ->
	let bar_width = !default_width / num_vals in
	if bar_width = 0 then (num_vals,1) else (bar_width*num_vals, bar_width)
  in
  let impulse_thickness = max 1 (bar_width/4) in
  let vp = A.init ~w:(float_of_int width) ~h:(float_of_int height) ["Cairo"; "PNG"; outfile] in
  VP.title vp title;
  VP.xrange vp 0. (float_of_int num_vals);
  let ymax = match ymax with Some max -> max | None -> Array.fold_left max 0. ys in
  VP.yrange vp 0. ymax;
  VP.xlabel vp xlabel;
  VP.ylabel vp ylabel;
  A.Axes.box vp;
  let xs = Array.init num_vals (fun n -> 0.5 +. float_of_int n) in
  if style=`Bars || (style = `Auto  &&  bar_width > !max_disk_width)
    (* too wide for disks *)
  then A.Array.xy vp xs ys ~style:(`Bars 0.5) ~fill:true ~fillcolor:!color
  else if List.mem style [`Auto;`Disks;`Impulses;`Lollipops]
  then begin
    let old_line_width = VP.get_line_width vp in
    let old_color = VP.get_color vp in
    VP.set_line_width vp (float_of_int impulse_thickness);
    VP.set_color vp !color;
    if style <> `Disks then A.Array.xy vp xs ys ~style:(`Impulses);
    VP.set_line_width vp old_line_width;
    VP.set_color vp old_color;
    if List.mem style [`Disks;`Lollipops] || (style = `Auto  &&  bar_width > !min_disk_width)
      (* wide enough for disks *)
    then A.Array.xy vp xs ys ~style:(`Markers "O")
  end
  else invalid_arg "style must be one of `Auto,`Bars,`Disks,`Impulses,`Lollipops";
  A.close vp

let linecolors = A.Color.([| red; blue; green; orange; chocolate; magenta; gold; silver |])

let multiplot
    ?(outfile = "multiplot_out.png")
    ?(width = !default_width)
    ?(height = !default_height)
    ?(title = "Multiplot")
    ?(xlabel = "Argument size")
    ?(ylabel = "Time (s)")
    ?ymin
    ?ymax
    xs ydatas =
(* assert (!min_disk_width < !max_disk_width); *)
  let filename = if Filename.check_suffix outfile ".png" then outfile else outfile ^ ".png" in
  let num_xs = Array.length xs in
  assert (for_all (ylengths_equal num_xs) ydatas);
  Array.iter (fun n -> assert (xs.(n) < xs.(n+1))) (range0 (num_xs - 1));
  Array.iter assert_y_order ydatas;
  let vp = A.init ~w:(float_of_int width) ~h:(float_of_int height) ["Cairo"; "PNG"; filename] in
  VP.title vp title;
  let xs = Array.map float_of_int xs in (* turn xs to floats *)
  let log_x = xs.(num_xs - 1) /. xs.(0) > 100. in
  let xs = if log_x then Array.map log xs else xs in
  VP.xrange vp xs.(0) xs.(num_xs - 1);
  let ymin = ensure ymin (fold_fold min) (Array.map (fun yd -> yd.lows) ydatas) in
  let ymax = ensure ymax (fold_fold max) (Array.map (fun yd -> yd.highs) ydatas) in
  VP.yrange vp ymin ymax;
  VP.xlabel vp (if log_x then "log " ^ xlabel else xlabel);
  VP.ylabel vp ylabel;
  A.Axes.box vp;
  let plot_ydata color ydata =
    let edgecolor = A.Color.(add ~op:In (rgba 0. 0. 0. 0.3) color) in
    A.set_color vp (A.Color.rgba 0. 0. 0. 0.);
    let xs2 = Array.append xs (rev_arr xs) in
    let ys2 = Array.append ydata.highs (rev_arr ydata.lows) in
    A.Array.xy vp xs2 ys2 ~fill:true ~fillcolor:edgecolor ~style:`Lines;
    A.set_color vp color;
    A.Array.xy vp xs ydata.ests ~style:`Lines;
    let text_pos_x = xs.(Array.length xs - 1) in
    let text_pos_y = ydata.ests.(Array.length ydata.ests - 1) in
    A.Viewport.text vp text_pos_x text_pos_y ydata.fname;
  in
  for i = 0 to Array.length ydatas - 1 do
    plot_ydata linecolors.(i) ydatas.(i);
  done;
  A.close vp

(* FILE INPUT *)

let spc = Str.regexp " "
let split_line s = Str.split spc s

let floats_of_strings strs = Array.of_list (List.map float_of_string strs)
let   ints_of_strings strs = Array.of_list (List.map   int_of_string strs)

let xs_of_string text =
    match split_line text with
    | "x-values" :: x_strs ->
	(try ints_of_strings x_strs
	with Failure "int_of_string" -> failwith ("Bad int(s) in x line:\n" ^ text))
    | _ ->
	failwith "Bad x-data line.  Should be \"x-values\" <int>+."

let ys_of_string label line =
  match split_line line with
  | str :: strs when str = label ->
      (try floats_of_strings strs
      with Failure "float_of_string" -> failwith ("Bad float(s) in " ^ label ^ " line:\n" ^ line))
  | _ -> failwith ("Bad " ^ label ^ " line:\n" ^ line)

let read_data ic =
  let values = ref [] in
  try while true do
      let x = float_of_string (input_line ic) in
      values := x :: !values
    done; assert false
  with End_of_file -> Array.of_list (List.rev !values)

let input_ydata data_in =
  let fname = input_line data_in in
  try
    let ests  = ys_of_string "est" (input_line data_in) in
    let lows  = ys_of_string "lo"  (input_line data_in) in
    let highs = ys_of_string "hi"  (input_line data_in) in
    { fname = fname; ests = ests; lows = lows; highs = highs }
  with
    End_of_file -> failwith "Reached end of file while reading y data."

let read_2d_data ic =
  let xs = xs_of_string (input_line ic) in
  let ydatas = ref [] in
  try while true do ydatas := input_ydata ic :: !ydatas done; assert false
  with End_of_file -> xs, Array.of_list (List.rev !ydatas)

let rec more_and_string words =
  match words with
  | [] -> assert false
  | [word] -> " and " ^ word
  | word :: words -> ", " ^ word ^ more_and_string words

let and_string words =
  match words with
  | [] -> ""
  | [word] -> word
  | word :: words -> word ^ more_and_string words

let space = Str.regexp " "
let map3 f (x,y,z) = (f x, f y, f z)

let read_1d_data ic =
  let ret = ref [] in
  try while true do
      let name = input_line ic in
      let mp,ml,mh = match Str.split space (input_line ic) with [p;l;u] -> map3 float_of_string (p,l,u) | _ -> assert false in
      let _sp,_sl,_sh = match Str.split space (input_line ic) with [p;l;u] -> map3 float_of_string (p,l,u) | _ -> assert false in
      let data = Str.split space (input_line ic) |> List.map float_of_string |> Array.of_list in
      ret := (name,mp,ml,mh,data) :: !ret;
  done; assert false
  with End_of_file -> Array.of_list (List.rev !ret)

let comp_1d     ?(outfile = "multiplot_out.png")
    ?(width = !default_width)
    ?(height = !default_height)
    ?(title = "Time Comparison (lower is better)")
    ?(ylabel = "Time (s)")
    ?(ymin=0.)
    ?ymax
    resl =
(* assert (!min_disk_width < !max_disk_width); *)
  let filename = if Filename.check_suffix outfile ".png" then outfile else outfile ^ ".png" in
  let num_xs = Array.length resl in
  let vp = A.init ~w:(float_of_int width) ~h:(float_of_int height) ["Cairo"; "PNG"; filename] in
  VP.title vp title;
  VP.xrange vp (-0.5) (float num_xs -. 0.5);

  let ymax = ensure ymax (fold_fold max) (Array.map (fun (_,_,_,_,ys) -> ys) resl) in
  VP.yrange vp ymin ymax;
  VP.ylabel vp ylabel;
  A.Axes.box vp;
  let plot_res color i (name, point, lo, hi, ys) =
    let i_05 = float i -. 0.5 in
    let dotcolor = A.Color.(add ~op:In (rgba 0. 0. 0. 0.2) color) in
    let xs = Array.init (Array.length ys) (fun _ -> i_05 +. 0.1 +. Random.float 0.8) in
    A.set_color vp dotcolor;
    A.Array.xy vp xs ys;
    let bgcolor = A.Color.(add ~op:In (rgba 0. 0. 0. 0.3) color) in
    A.set_color vp bgcolor;
    let bg = AP.make() in
    AP.rectangle bg ~x:i_05 ~w:1. ~y:lo ~h:(hi-.lo);
    VP.stroke vp `Data bg;
    A.set_color vp A.Color.black;
    VP.set_line_width vp 1.0;
    let path = AP.make() in
    AP.move_to path ~x:i_05 ~y:point;
    AP.rel_line_to path ~x:1.0 ~y:0.0;
    VP.stroke vp `Data path;
    VP.text vp (float i) 0. name;
  in
  Array.iteri (fun i ri -> plot_res linecolors.(i) i ri) resl;
  A.close vp

let comp_1d_cdf ?(outfile = "multiplot_out.png")
    ?(width = !default_width)
    ?(height = !default_height)
    ?(title = "Time Comparison (lower is better)")
    ?(ylabel = "Time (s)")
    ?(ymin=0.)
    ?ymax
    resl =
(* assert (!min_disk_width < !max_disk_width); *)
  let filename = if Filename.check_suffix outfile ".png" then outfile else outfile ^ ".png" in
  let vp = A.init ~w:(float_of_int width) ~h:(float_of_int height) ["Cairo"; "PNG"; filename] in
  VP.title vp title;
  let _,_,_,_,d0 = resl.(0) in
  VP.xrange vp 0. (float (Array.length d0 - 1));
  VP.xlabel vp "CDF";
  Array.iter (fun (_,_,_,_,ys) -> Array.sort (fun x (y:float) -> Pervasives.compare x y) ys) resl;
  let ymax = ensure ymax (Array.fold_left max 0.) (Array.map (fun (_,_,_,_,ys) -> ys.(Array.length ys - 1)) resl) in
  VP.yrange vp ymin ymax;
  VP.ylabel vp ylabel;
  A.Axes.box vp;
  let plot_res color i (name, _point, _lo, _hi, ys) =
    A.set_color vp color;
    A.Array.y vp ~style:`Lines ys;
    let l = Array.length ys in
    let text_pos_x = Random.int (l*2/3) + (l/6) in
    let text_pos_y = ys.(text_pos_x) in
    VP.text vp (float text_pos_x) text_pos_y name;
  in
  Array.iteri (fun i ri -> plot_res linecolors.(i) i ri) resl;
  A.close vp

let () =
  let infile,outfile =
    match Sys.argv with
    | [|_|] -> "lm.out", "lm.out.png"
    | [|_;infile|] -> infile, infile^".png"
    | [|_;infile;outfile|] -> infile,outfile
    | _ -> failwith ("Usage:  " ^ Sys.argv.(0) ^ " [<input filename> [<output filename>]]\n")
  in
  let ic = try open_in infile with _ -> failwith ("Could not open " ^ infile) in
  match input_line ic with
    | "flat" ->
      read_data ic |> plot ~outfile
    | "multiplot" ->
      let (xs,ydatas) = read_2d_data ic in
      let funcnames = Array.to_list (Array.map (fun x -> x.fname) ydatas) in
      let title = "Comparison of " ^ and_string funcnames in
      multiplot xs ydatas ~title:title ~outfile
    | "comparison" ->
      read_1d_data ic |> comp_1d ~outfile
    | _ -> failwith "Unknown file format";
