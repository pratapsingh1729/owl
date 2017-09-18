(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2017 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** This module contains constants and helper functions used in Owl OpenCL. *)

open Ctypes

open Owl_opencl_generated


(** constant definition *)

let magic_null = Obj.magic null

let uint32_0 = Unsigned.UInt32.zero

let uint32_1 = Unsigned.UInt32.one

let size_0 = Unsigned.Size_t.zero


(** coerce from type a to type b *)

let char_ptr_to_uint32_ptr x = coerce (ptr char) (ptr uint32_t) x

let char_ptr_to_size_t_ptr x = coerce (ptr char) (ptr size_t) x

let char_ptr_to_ulong_ptr x = coerce (ptr char) (ptr ulong) x

let char_ptr_to_cl_device_id_ptr x = coerce (ptr char) (ptr cl_device_id) x

let char_ptr_to_cl_platform_id_ptr x = coerce (ptr char) (ptr cl_platform_id) x


(* ends here *)