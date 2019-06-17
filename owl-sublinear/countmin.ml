(* TODO separate this into a different file *)
module Countmin_table_sig = struct 
  (* A module type for the tables used to implement the CountMin sketch. *)
  module type Sig = sig 
    (* the underlying table *)
    type t

    (* init l w generates a table with length l and width w, all counters initialized to 0 *)
    val init : int -> int -> t

    (* incr i j t increments the counter at length index i and width index j in table t *)
    val incr : int -> int -> t -> unit

    (* get i j t gets the value of the counter at length index i and width index j in table t*)
    val get : int -> int -> t -> int
  end

  (* Implementation of the CountMin sketch table using OCaml native arrays *)
  module Native_table : (Sig with type t = int array array) = struct
    type t = int array array

    let init l w = Array.make_matrix l w 0

    let incr i j t = t.(i).(j) <- t.(i).(j) + 1

    let get i j t = t.(i).(j)
  end

  (* Implementation of the CountMin sketch table using Owl ndarrays *)
  module Owl_table : (Sig with type t = Owl.Arr.arr) = struct  
    type t = Owl.Arr.arr

    let init l w = Owl.Arr.zeros [|l;w|]

    let incr i j t = Owl.Arr.set t [|i;j|] ((Owl.Arr.get t [|i;j|]) +. 1.)

    let get i j t = Owl.Arr.get t [|i;j|] |> int_of_float
  end
end

module type Countmin_sig = sig
  type sketch
  val init : float -> float -> sketch
  val incr : sketch -> int -> unit
  val count : sketch -> int -> int
end

(* Functor to make the CountMin sketch using a specified underlying table *)
module Make (T : Countmin_table_sig.Sig) : Countmin_sig = struct
  (* the type of sketches *)
  type sketch = 
    {tbl : T.t ; 
     w : int ; 
     hash_fns : (int * int * int) array}

  (* the prime number to use for hashing *)
  let bigprime = (1 lsl 31) - 1

  (* Calculate (a*x + b) mod (2^31 - 1) *)
  let hash31 x a b =
    let result = (a * x) + b in
    ((result lsr 31) + result) land bigprime

  (* Initialize a sketch with l hash functions, each with w buckets *)
  let init_lw l w =
    let gen_rand_int _ = Owl.Stats.uniform_int_rvs ~a:0 ~b:bigprime in
    let gen_rand_pair i = (i, gen_rand_int (), gen_rand_int ()) in
    {tbl = T.init l w ;
     w = w ;
     hash_fns = Array.init l gen_rand_pair}

  (* Initialize a sketch with approximation ratio 1 + epsilon 
   * and failure probability delta *)
  let init epsilon delta =
    (* set l = log (1/delta) and w = 1/epsilon *)
    init_lw (Owl.Maths.(1. /. delta |> log2 |> ceil |> int_of_float))
            (Owl.Maths.(1. /. epsilon |> ceil |> int_of_float))
  
  (* increment the count of x in sketch s *)
  let incr s x = 
    let iterfn (i, ai, bi) = T.incr i ((hash31 x ai bi) mod s.w) s.tbl in
    Array.iter iterfn s.hash_fns

  (* get the current estimate of the count of x in sketch s *)
  let count s x = 
    let foldfn prv (i, ai, bi) =
      T.get i ((hash31 x ai bi) mod s.w) s.tbl |> min prv in
    Array.fold_left foldfn max_int s.hash_fns
end

module Countmin_native = Make(Countmin_table_sig.Native_table)
module Countmin_owl = Make(Countmin_table_sig.Owl_table)

(* Priority queue implementation for finding heavy hitters.  From
 * http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html.
 * Changed priority type to float and added function to get all 
 * elements in sorted order *)
module PrioQueue = struct
  type priority = float
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
  exception Queue_is_empty

  let empty = Empty

  let rec insert queue prio elt =
    match queue with
    | Empty -> Node(prio, elt, Empty, Empty)
    | Node(p, e, left, right) ->
        if prio <= p
        then Node(prio, elt, insert right p e, left)
        else Node(p, e, insert right prio elt, left)
  
  let rec remove_top = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                      (Node(rprio, relt, _, _) as right)) ->
        if lprio <= rprio
        then Node(lprio, lelt, remove_top left, right)
        else Node(rprio, relt, left, remove_top right)
  
  (* get the min element of the queue, its priority, and the rest of the queue *)
  let extract = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)

  (* return a list of the elements in descending order of priority (note max first) *)
  let to_sorted_list = 
    let rec aux acc queue = 
      try
        let prio, elt, rest = extract queue in
        aux ((elt, prio) :: acc) rest
      with 
      | Queue_is_empty -> acc
    in aux []
end

(* simple test - add a bunch of elements with skewed distribution to the sketch then count them *)
let simple_test_countmin eps del =
  let module CM = Countmin_native in
  let s = CM.init eps del in
  for x = 1 to 30 do
    Printf.printf "expected count of %2d : %5d \n" x (10000 / x) ;
    for i = 1 to 10000 / x do
      CM.incr s x
    done
  done;
  for x = 1 to 30 do
    CM.count s x |> Printf.printf "count of %2d : %5d \n" x
  done

(* Test the countmin sketch by putting n samples from the function distr
 * into a sketch and into the hashtable-based naive frequency counter, then
 * comparing their outputs *)
let test_countmin_hashtbl distr eps del n =
  let module CM = Countmin_native in 
  let ht_count t x = 
    match Hashtbl.find_opt t x with
    | Some c -> c
    | None -> 0 in
  let ht_incr t x = Hashtbl.replace t x ((ht_count t x) + 1) in
  let s = CM.init eps del in
  let t = Hashtbl.create n in
  for i = 1 to n do
    let v = distr () in
    CM.incr s v ; ht_incr t v
  done;
  let foldfn v ct lst = (v, ct, CM.count s v) :: lst in
  let outputs = Hashtbl.fold foldfn t [] |> List.sort (fun (_,a,_) (_,b,_) -> a - b) |> Array.of_list in
  let diffs = Array.map (fun (_, tct, sct) -> (float_of_int (sct - tct)) /. (float_of_int tct)) outputs in
  let diffs_mat = 
    Owl.Mat.init_2d (Array.length diffs) 1 (fun i _ -> diffs.(i)) in
  let open Owl_plplot in
  let h = Plot.create "diffs.png" in
  Plot.histogram ~h ~bin:200 (diffs_mat) ;
  Plot.output h ;
  outputs, diffs