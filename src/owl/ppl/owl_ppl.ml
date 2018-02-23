(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2018 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Experimental: Probabilistic Programming *)

open Owl_types

open Owl_graph


module Make (A : Stats_Dist) = struct

  include Owl_distribution.Make (A)

  include Owl_lazy.Make (A)


  (* graph manipulation *)

  (* Draw random variables from different distributions. *)

  let uniform ~a ~b =
    let draw_samples args =
      let a = to_arr args.(0) in
      let b = to_arr args.(1) in
      (*let s = A.shape (to_arr args.(2)) in*)
      let n = 0 in
      let t = Uniform.make ~a ~b in
      of_arr (Uniform.sample t n)
    in
    let shape_holder = variable () in (* FIXME *)
    map ~name:"uniform" draw_samples [|a;b;shape_holder|]

  let gaussian ~mu ~sigma =
    let draw_samples args =
      let mu = to_arr args.(0) in
      let sigma = to_arr args.(1) in
      (*let s = A.shape (to_arr args.(2)) in*)
      let n = 0 in
      let t = Gaussian.make ~mu ~sigma in
      of_arr (Gaussian.sample t n)
    in
    let shape_holder = variable () in (* FIXME *)
    map ~name:"gaussian" draw_samples [|mu;sigma;shape_holder|]


  let sample x s =
    invalidate x;
    Array.iter (fun n ->
      assign_arr n (A.empty s)
    ) (get_by_name x "variable");
    eval x;
    x

  let infer x = ()


  (* Mathematical operators *)


end