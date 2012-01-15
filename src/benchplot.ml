
(* TODO
 * customize box
 *
 * Styles:  `Auto,`Bars,`Disks,`Impulses,`Lollipops
 *)

module A = Archimedes
module VP = A.Viewport
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


type ydata = { fname: string; ys: float array; lows: float array; highs: float array }

let ylengths_equal n x = for_all ((=) n) (Array.map Array.length [|x.lows;x.ys;x.highs|])

let assert_y_order ydata =
  Array.iteri (fun n y -> assert (y > ydata.lows.(n) && y < ydata.highs.(n))) ydata.ys

let fold_fold f yss = fold f (Array.map (fold f) yss)

let plot
    ?(filename="plot_out.png")
    ?width:target_width
    ?(height = !default_height)
    ?(title="BatBench plot")
    ?(xlabel="Samples")
    ?(ylabel="Time")
    ?ymax
    ?(style=`Auto)
    ys =
  assert (!min_disk_width < !max_disk_width);
  let filename = if Filename.check_suffix filename ".png" then filename else filename ^ ".png" in
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
  let vp = A.init ~w:(float_of_int width) ~h:(float_of_int height) ["Cairo"; "PNG"; filename] in
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

let linecolors = A.Color.([| hot_pink; orange; blue; yellow; chocolate; green; black |])

let multiplot
    ?(filename = "multiplot_out.png")
    ?(width = !default_width)
    ?(height = !default_height)
    ?(title = "Multiplot")
    ?(xlabel = "Argument value")
    ?(ylabel = "Time (s)")
    ?ymin
    ?ymax
    (xs, ydatas) =
(* assert (!min_disk_width < !max_disk_width); *)
  let filename = if Filename.check_suffix filename ".png" then filename else filename ^ ".png" in
  let num_xs = Array.length xs in
  assert (for_all (ylengths_equal num_xs) ydatas);
  Array.iter (fun n -> assert (xs.(n) < xs.(n+1))) (range0 (num_xs - 1));
  Array.iter assert_y_order ydatas;
  let vp = A.init ~w:(float_of_int width) ~h:(float_of_int height) ["Cairo"; "PNG"; filename] in
  VP.title vp title;
  let xs = Array.map float_of_int xs in (* turn xs to floats *)
  let xs = if xs.(num_xs - 1) /. xs.(0) < 100. then xs else Array.map log xs in
  VP.xrange vp xs.(0) xs.(num_xs - 1);
  let ymin = ensure ymin (fold_fold min) (Array.map (fun yd -> yd.lows) ydatas) in
  let ymax = ensure ymax (fold_fold max) (Array.map (fun yd -> yd.highs) ydatas) in
  VP.yrange vp ymin ymax;
  VP.xlabel vp xlabel;
  VP.ylabel vp ylabel;
  A.Axes.box vp;
  let plot_ydata color ydata =
    let edgecolor = A.Color.(add ~op:In (rgba 0. 0. 0. 0.3) color) in
    A.set_color vp edgecolor;
    let xs2 = Array.append xs (rev_arr xs) in
    let ys2 = Array.append ydata.highs (rev_arr ydata.lows) in
    A.Array.xy vp xs2 ys2 ~fill:true ~fillcolor:edgecolor ~style:`Lines;
    A.set_color vp color;
    A.Array.xy vp xs ydata.ys ~style:`Lines;
    let text_pos_x = xs.(Array.length xs - 1) in
    let text_pos_y = ydata.ys.(Array.length ydata.ys - 1) in
    A.Viewport.text vp text_pos_x text_pos_y ydata.fname;
  in
  for i = 0 to Array.length ydatas - 1 do
    plot_ydata linecolors.(i) ydatas.(i);
  done;
  A.close vp

let read_data fn =
  let ic = try open_in fn with _ -> failwith ("Could not open " ^ fn) in
  let values = ref [] in
  try while true do
      let x = float_of_string (input_line ic) in
      values := x :: !values
    done; assert false
  with End_of_file -> Array.of_list (List.rev !values)

let spc = Str.regexp " "
let split_line s = Str.split spc s

let read_2d_data fn =
  let ic = try open_in fn with _ -> failwith ("Could not open " ^ fn) in
  let l1 = input_line ic in
  assert (l1 = "x-values");
  let xs = input_line ic |> split_line |> List.map int_of_string |> Array.of_list in
  let values = ref [] in
let 
  try while true do
      let fname = input_line ic in
      let ys = input_line ic |> split_line |> List.tl |> List.map float_of_string |> Array.of_list in
      let lows = input_line ic |> split_line |> List.tl |> List.map float_of_string |> Array.of_list in
      let highs = input_line ic |> split_line |> List.tl |> List.map float_of_string |> Array.of_list in
      values := {fname; ys; lows; highs} :: !values
    done; assert false
  with End_of_file -> xs, Array.of_list (List.rev !values)

let () =
  let num_args = Array.length Sys.argv in
  if num_args > 3 then
    failwith ("Usage:  " ^ Sys.argv.(0) ^ " [input filename [output filename]]\n")
  else let infile = match num_args with
  | 1 -> "lm.out" | 2 | 3 -> Sys.argv.(1) | _ -> assert false
  in let outfile = match num_args with
  | 1 | 2 -> infile^".png" | 3 -> Sys.argv.(2) | _ -> assert false
  in
  let ys = read_2d_data infile in
  multiplot ys ~filename:outfile

(*  ( try
    let ys = read_data "times.flat" in
    plot ys ~filename:"times.png"
    with _ -> () );
  ( try *)
