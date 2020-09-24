
(* This example demonstrates the use of the HeavyHitters sketch,
 * which is based on the Count-Min sketch in Owl_base library.  
 * This example finds the words which appear with relative frequency
 * at least 1% in the news.txt corpus at https://github.com/ryanrhymes/owl_dataset.
 * WARNING: This example will download the file news.txt.gz (96.5MB) onto
 * your machine and expand it into news.txt (340.3MB).
 *)

module CM = Owl_base.Countmin_sketch.Native

let get_corpus () = 
  let fn = "news.txt" in
  if not (Sys.file_exists (Owl_dataset.local_data_path () ^ fn)) then
    Owl_dataset.download_data (fn ^ ".gz");
  open_in (Owl_dataset.local_data_path () ^ fn)

let get_line_words inch =
  let regexp = Str.regexp "[^A-Za-z]+" in
  try
    Some ((input_line inch) |> Str.split regexp)
  with
    End_of_file -> None

let fill_sketch inch epsilon delta =
  let c = CM.init ~epsilon ~delta in
  let rec aux () =
    match get_line_words inch with
    | Some lst -> List.iter (CM.incr c) lst; aux ()
    | None -> c in
  aux ()

let _ =
  let inch = get_corpus () in
  let c = fill_sketch inch 0.001 0.001 in
  let words = ["the"; "and"; "of"; "said"; "floccinaucinihilipilification"] in
  List.iter (fun word -> Printf.printf "%s: %d\n" word (CM.count c word)) words
  