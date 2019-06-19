module type Sig = sig
  type sketch

  val init : float -> float -> sketch
  val incr : sketch -> int -> unit
  val count : sketch -> int -> int
end

(* Functor to make the CountMin sketch using a specified underlying table *)
module Make (T : Owl_countmin_table.Sig) : Sig = struct
  (* the type of sketches *)
  type sketch =
    { tbl : T.t
    ; w : int
    ; hash_fns : (int * int * int) array
    }

  (* the prime number to use for hashing *)
  let bigprime = (1 lsl 31) - 1

  (* Calculate (a*x + b) mod (2^31 - 1) *)
  let hash31 x a b =
    let result = (a * x) + b in
    ((result lsr 31) + result) land bigprime


  (* Initialize a sketch with l hash functions, each with w buckets *)
  let init_lw l w =
    let gen_rand_int _ = 
      Owl_base_stats_dist_uniform.uniform_int_rvs ((1 lsl 30) - 1) in
    let gen_rand_pair i = i, gen_rand_int (), gen_rand_int () in
    { tbl = T.init l w; w; hash_fns = Array.init l gen_rand_pair }


  (* Initialize a sketch with approximation ratio 1 + epsilon 
   * and failure probability delta *)
  let init epsilon delta =
    (* set l = log (1/delta) and w = 1/epsilon *)
    init_lw
      Owl_base_maths.(1. /. delta |> log2 |> ceil |> int_of_float)
      Owl_base_maths.(1. /. epsilon |> ceil |> int_of_float)


  (* increment the count of x in sketch s *)
  let incr s x =
    let iterfn (i, ai, bi) = T.incr i (hash31 x ai bi mod s.w) s.tbl in
    Array.iter iterfn s.hash_fns


  (* get the current estimate of the count of x in sketch s *)
  let count s x =
    let foldfn prv (i, ai, bi) = T.get i (hash31 x ai bi mod s.w) s.tbl |> min prv in
    Array.fold_left foldfn max_int s.hash_fns
end

module Native = Make (Owl_countmin_table.Native)
module Owl = Make (Owl_countmin_table.Owl)
