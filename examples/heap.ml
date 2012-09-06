(* Binomial heap
 * http://en.wikipedia.org/wiki/Binomial_heap
 *)
module BinHeap : sig
  type ('a, 'b) tree = private {
    key : 'a;
    data : 'b;
    order : int;
    trees : ('a, 'b) tree list;
  }
  type ('a, 'b) t = private ('a, 'b) tree list
  val merge : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val find_min_pair : ('a, 'b) t -> 'a * 'b
  val find_min_key : ('a, 'b) t -> 'a
  val find_min_data : ('a, 'b) t -> 'b
  val delete_min : ('a, 'b) t -> ('a, 'b) t
  val make : unit -> ('a, 'b) t
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  val print : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
end = struct
  type ('a, 'b) tree = {
    key : 'a;
    data : 'b;
    order : int;
    trees : ('a, 'b) tree list;
  }

  type ('a, 'b) t = ('a, 'b) tree list

  let make_tree key data = { key; data; order = 0; trees = []; }

  let min_tree a b = if a.key <= b.key then a else b
  let min_tree2 a b = if a.key <= b.key then (a, b) else (b, a)

  let merge_same a b =
    assert (a.order = b.order);
    let (a, b) = min_tree2 a b in
    { a with
      order = a.order + 1;
      trees = b :: a.trees;
    }

  let merge x y =
    let merge_carry = function
      | [] -> ([], [])
      | [x] -> ([x], [])
      | x :: y :: z -> (z, [merge_same x y])
    in
    let rec loop acc x y order c =
      if (x = []) && (y = [])
      then
	let (e, c) = merge_carry c in
	List.rev (c @ e @ acc)
      else
	match x with
	  | x::xs when x.order = order ->
	    loop acc xs y order (x::c)
	  | x ->
	    match y with
	      | y::ys when y.order = order ->
		let (e, c) = merge_carry (y::c)
		in
		loop (e @ acc) x ys (order + 1) c
	      | y ->
		let (e, c) = merge_carry c
		in
		loop (e @ acc) x y (order + 1) c
    in
    loop [] x y 0 []

  let find_min_tree t =
    match t with
      | [] -> raise Not_found
      | x::xs ->
	List.fold_left
	  (fun elt y -> min elt y)
	  x
	  xs

  let find_min_pair t =
    let min_tree = find_min_tree t
    in
    (min_tree.key, min_tree.data)

  let find_min_key t =
    let min_tree = find_min_tree t
    in
    min_tree.key

  let find_min_data t =
    let min_tree = find_min_tree t
    in
    min_tree.data

  let delete_min t =
    let min_tree = find_min_tree t in
    let rec loop = function
      | [] -> []
      | x::xs when x == min_tree -> loop xs
      | x::xs -> x :: (loop xs)
    in
    merge (loop t) (List.rev min_tree.trees)

  let rec print_tree printer level t =
    let indent = String.make level ' ' in
    Printf.printf "%sTree { " indent;
    printer t.key t.data;
    Printf.printf ", order = %d" t.order;
    match t.trees with
      | [] -> Printf.printf "}\n";
      | trees ->
	Printf.printf "\n";
	List.iter (print_tree printer (level + 1)) trees;
	Printf.printf "%s}\n" indent

  let make () = []

  let insert t key data = merge t [make_tree key data]

  let print printer t =
    Printf.printf "Heap {\n";
    List.iter (print_tree printer 1) t;
    Printf.printf "}\n";
    flush_all ()
end

(* Fibonacci Heap http://en.wikipedia.org/wiki/Fibonacci_heap
 *
 * Find-minimum is O(1) amortized time. Operations insert, decrease
 * key, and merge (union) work in constant amortized time. Operations
 * delete and delete minimum work in O(log n) amortized time.
 *
 * Note: decrease key not implemented and simplified
 * Note: merge a b in O(log |b|) time because no double linked children
 * Note: merge is called internally only with |b| = 1
 *)
module FibHeap : sig
  type ('a, 'b) node = private {
    key : 'a;
    data : 'b;
    degree : int;
    children : ('a, 'b) node list;
  }
  type ('a, 'b) t = ('a, 'b) node list
  val merge : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val find_min_pair : ('a, 'b) t -> 'a * 'b
  val find_min_key : ('a, 'b) t -> 'a
  val find_min_data : ('a, 'b) t -> 'b
  val delete_min : ('a, 'b) t -> ('a, 'b) t
  val make : unit -> ('a, 'b) t
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  val print : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
end = struct
  type ('a, 'b) node = {
    key : 'a;
    data : 'b;
    degree : int;
    children : ('a, 'b) node list;
  }

  let make_node key data = { key; data; degree = 0; children = []; }

  type ('a, 'b) t = ('a, 'b) node list

  let make () = []

  (* switch a and b so the minimum key is first *)
  let sort2 a b = if a.key <= b.key then (a, b) else (b, a)

  (* merge nodes a and b keeping the minimum key at the front *)
  let merge a b =
    match a with
      | [] -> b
      | x::xs ->
	let (x, xs) =
	  List.fold_left
	    (fun (x, xs) y ->
	      let (x, y) = sort2 x y
	      in
	      (x, y::xs))
	    (x, xs)
	    b
	in
	x::xs

  let add_child node child =
    { node with
      degree = node.degree + 1;
      children = child :: node.children;
    }

  let find_min = function
    | [] -> raise Not_found
    | x::_ -> x

  let find_min_pair t =
    let e = find_min t
    in
    (e.key, e.data)

  let find_min_key t =
    let e = find_min t
    in
    e.key

  let find_min_data t =
    let e = find_min t
    in
    e.data

  let insert t key data = merge t [make_node key data]

  let rec print_node printer level node =
    let indent = String.make level ' ' in
    Printf.printf "%sNode { degree = %d, "
      indent node.degree;
    printer node.key node.data;
    begin
      match node.children with
	| [] -> Printf.printf "}\n"
	| _ ->
	  Printf.printf "\n";
	  List.iter (print_node printer (level + 1)) node.children;
	  Printf.printf "%s}\n" indent;
    end;
    flush_all ()

  let print printer = function
    | [] -> Printf.printf "<empty>\n"
    | (min_elt::_) as roots ->
      Printf.printf "Tree { min_elt = ";
      printer min_elt.key min_elt.data;
      Printf.printf "\n";
      List.iter
	(fun node -> print_node printer 1 node)
	roots;
      Printf.printf "}\n";
      flush_all ()

  let balance ?(extra_nodes=[]) roots =
    (* Merge roots of equal size *)
    let h = Hashtbl.create 0 in
    let rec loop node =
      try
	let node2 = Hashtbl.find h node.degree
	in
	Hashtbl.remove h node.degree;
	let (node, node2) = sort2 node node2 in
	let node = add_child node node2
	in
	loop node
      with Not_found ->
	Hashtbl.add h node.degree node
    in
    List.iter loop roots;
    List.iter loop extra_nodes;
    (* Convert hashtbl to roots and find min_elt *)
    Hashtbl.fold
      (fun _ node nodes -> merge nodes [node])
      h
      []

  let delete_min = function
    | [] -> raise (Invalid_argument "empty tree");
    | min_elt::roots ->
      (* Make chilren of min_elt new roots *)
      let nodes = min_elt.children
      in
      balance ~extra_nodes:nodes roots
end


module FHeap = struct
  module Heap = FibHeap

  type ('a, 'b) t = {
    mutable length : int;
    mutable heap : ('a, 'b) Heap.t;
  }

  let make () = {
    length = 0;
    heap = Heap.make ();
  }

  let length q = q.length

  let add q key data =
    q.length <- q.length + 1;
    q.heap <- Heap.insert q.heap key data

  let peek_key q = Heap.find_min_key q.heap

  let take_pair q =
    let pair = Heap.find_min_pair q.heap in
    q.length <- q.length - 1;
    q.heap <- Heap.delete_min q.heap;
    pair

  let take q =
    let data = Heap.find_min_data q.heap in
    q.length <- q.length - 1;
    q.heap <- Heap.delete_min q.heap;
    data

  let is_empty q = q.length = 0

  let print printer q =
    Printf.printf "PriQueue: length = %d\n" q.length;
    Heap.print printer q.heap
end

module BHeap = struct
  module Heap = FibHeap

  type ('a, 'b) t = {
    mutable length : int;
    mutable heap : ('a, 'b) Heap.t;
  }

  let make () = {
    length = 0;
    heap = Heap.make ();
  }

  let length q = q.length

  let add q key data =
    q.length <- q.length + 1;
    q.heap <- Heap.insert q.heap key data

  let peek_key q = Heap.find_min_key q.heap

  let take_pair q =
    let pair = Heap.find_min_pair q.heap in
    q.length <- q.length - 1;
    q.heap <- Heap.delete_min q.heap;
    pair

  let take q =
    let data = Heap.find_min_data q.heap in
    q.length <- q.length - 1;
    q.heap <- Heap.delete_min q.heap;
    data

  let is_empty q = q.length = 0

  let print printer q =
    Printf.printf "PriQueue: length = %d\n" q.length;
    Heap.print printer q.heap
end



let testb r n =
  for i = 1 to n do
    for j = 1 to r do
