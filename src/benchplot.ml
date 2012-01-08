
(* TODO
 * local colors
 * customize box
 * font size & posn of title & axis labels
 *)

module A = Archimedes
module VP = A.Viewport

let default_width  = ref 650
let default_height = ref 450

let color = ref A.Color.blue

let may f = function None -> () | Some v -> f v

let plot
    ?(filename="plot_out.png")
    ?width:target_width
    ?(height = !default_height)
    ?(title="BatBench plot")
    ?(xlabel="Samples")
    ?(ylabel="Time")
    ?ymax
    ys =
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
  VP.xlabel vp xlabel;
  VP.ylabel vp ylabel;
  may (VP.yrange vp 0.0) ymax;
  (* VP.ylabel vp ylabel; *)
  A.Axes.box vp;
  let xs = Array.init num_vals (fun n -> 0.5 +. float_of_int n) in
  if bar_width > 20 then             (* too wide for impulses *)
    A.Array.xy vp xs ys ~style:(`Bars 0.5) ~fill:true ~fillcolor:!color
  else begin
    let old_line_width = VP.get_line_width vp in
    let old_color = VP.get_color vp in
    VP.set_line_width vp (float_of_int impulse_thickness);
    VP.set_color vp !color;
    A.Array.xy vp xs ys ~style:(`Impulses);
    VP.set_line_width vp old_line_width;
    VP.set_color vp old_color;
    if bar_width > 5 then A.Array.xy vp xs ys;  (* wide enough for dot markers *)
  end;
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
  let ys = read_data "times.flat" in
  plot ~filename:"times.png" ys
