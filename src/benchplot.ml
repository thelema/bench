
(* TODO
 * customize box
 *
 * Styles:  `Auto,`Bars,`Disks,`Impulses,`Lollipops
 *)

module A = Archimedes
module VP = A.Viewport

let default_width  = ref 650
let default_height = ref 450

let color = ref A.Color.blue

let min_disk_width = ref 10
let max_disk_width = ref 20

let may f = function None -> () | Some v -> f v

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

let read_data fn =
  let ic = try open_in fn with _ -> failwith ("Could not open " ^ fn) in
  let values = ref [] in
  try while true do
      let x = float_of_string (input_line ic) in
      values := x :: !values
    done; assert false
  with End_of_file -> Array.of_list (List.rev !values)

let () =
  let num_args = Array.length Sys.argv in
  let infile =
    match num_args with
    | 1 -> "times.flat"
    | 2 | 3 -> Sys.argv.(1)
    | _ ->
	let usage_string =  "Usage:  " ^ Sys.argv.(0) ^ " [input filename [output filename]]\n" in
	(prerr_string usage_string; exit 1)
  in
  let outfile =
    match num_args with
    | 1 -> "times.png"
    | 2 -> infile ^ ".png"
    | 3 -> Sys.argv.(2)
    | _ -> assert false
  in
  let ys = read_data infile in
  plot ys ~filename:outfile
