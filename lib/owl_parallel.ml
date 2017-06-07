(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2017 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(* Experimental module, do not use now *)


module type Mapre_Engine = sig

  val map : ('a -> 'b) -> string -> string

  val map_partition: ('a list -> 'b list) -> string -> string

  val union : string -> string -> string

  val reduce : ('a -> 'a -> 'a) -> string -> 'a option

  val collect : string -> 'a list

  val workers : unit -> string list

  val myself : unit -> string

  val load : string -> string

  val save : string -> string -> int

end


module type Ndarray = sig

  type arr
  type elt

  val shape : arr -> int array

  val zeros : int array -> arr

  val ones  : int array -> arr

  val uniform : ?scale:float -> int array -> arr

  val numel : arr -> int

  val get : arr -> int array -> elt

  val set : arr -> int array -> elt -> unit

  val map : ?axis:int option array -> (elt -> elt) -> arr -> arr

  val map2 : ?axis:int option array -> (elt -> elt -> elt) -> arr -> arr -> arr

  val sin : arr -> arr

  val cos : arr -> arr

  val sum : arr -> elt

  val add : arr -> arr -> arr

  val sub : arr -> arr -> arr

  val mul : arr -> arr -> arr

  val div : arr -> arr -> arr

end


module Make_Distributed (M : Ndarray) (E : Mapre_Engine) = struct

  type distr_arr = {
    mutable id      : string;
    mutable shape   : int array;
    mutable c_start : int array;
    mutable c_len   : int array;
    mutable workers : string array;
  }

  let make_distr_arr id shape c_start c_len workers = {
    id;
    shape;
    c_start;
    c_len;
    workers;
  }

  let shape x = x.shape

  let num_dims x = Array.length x.shape

  let numel x = Array.fold_left (fun a b -> a * b) 1 x.shape

  let divide_to_chunks shape n =
    let total_sz = Array.fold_left (fun a b -> a * b) 1 shape in
    let chunk_sz = total_sz / n in
    match chunk_sz = 0 with
    | true  -> [| (0, total_sz) |]
    | false -> (
        Array.init n (fun i ->
          let c_start = i * chunk_sz in
          let c_len = match i = n - 1 with
            | true  -> total_sz - c_start
            | false -> chunk_sz
          in
          c_start, c_len
        )
      )

  (* make a distributed version of [create_fun d], the elements will be
  distributed among the working nodes. *)
  let distributed_create create_fun d =
    let workers = E.workers () in
    let chunks = divide_to_chunks d (List.length workers) in
    let c_start = Array.map fst chunks in
    let c_len = Array.map snd chunks in
    let id = E.map (fun _ ->
      let me = E.myself () in
      let pos = Owl_utils.list_search me workers in
      me, create_fun [|c_len.(pos)|]
    ) ""
    in
    make_distr_arr id d c_start c_len (Array.of_list workers)

  let of_ndarray x = None

  let to_ndarray x = None

  (* given 1d index, calculate its owner *)
  let calc_index_owner i_1d c_start c_len =
    let i = ref 0 in
    try
      for j = 0 to Array.length c_start - 1 do
        i := j;
        let a = c_start.(j) in
        let b = c_start.(j) + c_len.(j) in
        if a <= i_1d && i_1d < b then failwith "found"
      done;
      !i
    with exn -> !i

  let get x i_nd =
    let stride = Owl_dense_common._calc_stride x.shape in
    let i_1d = Owl_dense_common._index_nd_1d i_nd stride in
    assert (numel x > i_1d);
    let pos = calc_index_owner i_1d x.c_start x.c_len in
    let owner_id = x.workers.(pos) in
    (* the offset in the owner's chunk *)
    let j_1d = i_1d - x.c_start.(pos) in
    let y_id = E.map_partition (fun l ->
      match E.myself () = owner_id with
      | true  -> (
          let arr = List.nth l 0 |> snd in
          [ M.get arr [|j_1d|] ]
        )
      | false -> []
    ) x.id
    in
    let l = E.collect y_id
      |> List.filter (fun l -> List.length l > 0)
    in
    List.(nth (nth l 0) 0)

  let set x i_nd a =
    let stride = Owl_dense_common._calc_stride x.shape in
    let i_1d = Owl_dense_common._index_nd_1d i_nd stride in
    assert (numel x > i_1d);
    let pos = calc_index_owner i_1d x.c_start x.c_len in
    let owner_id = x.workers.(pos) in
    (* the offset in the owner's chunk *)
    let j_1d = i_1d - x.c_start.(pos) in
    let y_id = E.map_partition (fun l ->
      match E.myself () = owner_id with
      | true  -> (
          let arr = List.nth l 0 |> snd in
          [ M.set arr [|j_1d|] a ]
        )
      | false -> []
    ) x.id
    in
    E.collect y_id |> ignore

  let init d = None

  let zeros d =
    let create_fun d = M.zeros d in
    distributed_create create_fun d

  let ones d =
    let create_fun d = M.ones d in
    distributed_create create_fun d

  let sequential d = None

  let uniform ?scale d =
    let create_fun d = M.uniform ?scale d in
    distributed_create create_fun d

  let gaussian d = None

  let map_chunk f x =
    let y_id = E.map (fun (node_id, arr) ->
      node_id, f arr
    ) x.id
    in
    let shape = Array.copy x.shape in
    let c_start = Array.copy x.c_start in
    let c_len = Array.copy x.c_len in
    let workers = Array.copy x.workers in
    make_distr_arr y_id shape c_start c_len workers

  let map f x = map_chunk (M.map f) x

  let map2_chunk f x y =
    assert (x.shape = y.shape);
    let z_id = E.union x.id y.id in
    let z_id = E.map_partition (fun l ->
      let x_node_id, x_arr = List.nth l 0 in
      let y_node_id, y_arr = List.nth l 1 in
      [ (x_node_id, f x_arr y_arr) ]
    ) z_id
    in
    let shape = Array.copy x.shape in
    let c_start = Array.copy x.c_start in
    let c_len = Array.copy x.c_len in
    let workers = Array.copy x.workers in
    make_distr_arr z_id shape c_start c_len workers

  let map2 f x y = map2_chunk (M.map2 f) x y

  let fold f x a =
    let y_id = E.map (fun (node_id, arr) ->
      let b = ref M.(get arr [|0|]) in
      for i = 1 to M.numel arr - 1 do
        b := f !b M.(get arr [|i|])
      done;
      !b
    ) x.id in
    E.collect y_id
    |> List.fold_left (fun b c -> f b (List.nth c 0)) a

  let sin x = map_chunk M.sin x

  let cos x = map_chunk M.cos x

  (* TODO: need to implement add_elt to support complex number *)
  let sum x =
    let y = map_chunk M.sum x in
    let l = E.collect y.id |> List.map (fun l' -> List.nth l' 0 |> snd) in
    let a = ref (List.nth l 0) in
    for i = 1 to List.length l - 1 do
      a := !a +. (List.nth l i)
    done;
    !a

  let min x = None

  let max x = None

  let add x y = map2_chunk M.add x y

  let sub x y = map2_chunk M.sub x y

  let mul x y = map2_chunk M.mul x y

  let div x y = map2_chunk M.div x y

  (* TODO: distributed garbage collection is not implemented yet. *)

end



module Make_Shared (M : Ndarray) (E : Mapre_Engine) = struct


end



module type Ndarray_Any = sig

  type 'a arr

  val shape : 'a arr -> int array

  val numel : 'a arr -> int

  val create : int array -> 'a -> 'a arr

end


module Make_Distributed_Any (M : Ndarray_Any) (E : Mapre_Engine) = struct

  type distr_arr = {
    mutable id      : string;
    mutable shape   : int array;
    mutable c_start : int array;
    mutable c_len   : int array;
    mutable workers : string array;
  }

  let make_distr_arr id shape c_start c_len workers = {
    id;
    shape;
    c_start;
    c_len;
    workers;
  }

  let divide_to_chunks shape n =
    let total_sz = Array.fold_left (fun a b -> a * b) 1 shape in
    let chunk_sz = total_sz / n in
    match chunk_sz = 0 with
    | true  -> [| (0, total_sz) |]
    | false -> (
        Array.init n (fun i ->
          let c_start = i * chunk_sz in
          let c_len = match i = n - 1 with
            | true  -> total_sz - c_start
            | false -> chunk_sz
          in
          c_start, c_len
        )
      )

  let distributed_create create_fun d =
    let workers = E.workers () in
    let chunks = divide_to_chunks d (List.length workers) in
    let c_start = Array.map fst chunks in
    let c_len = Array.map snd chunks in
    let id = E.map (fun _ ->
      let me = E.myself () in
      let pos = Owl_utils.list_search me workers in
      me, create_fun [|c_len.(pos)|]
    ) ""
    in
    make_distr_arr id d c_start c_len (Array.of_list workers)

end