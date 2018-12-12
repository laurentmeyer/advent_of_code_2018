let file_to_str_list filename =
  let ic = open_in filename in
  let rec aux lst =
    try
      let l = input_line ic in
      aux (l :: lst)
    with End_of_file -> begin close_in ic ; lst end
  in
  aux []

let explode str =
  let len = String.length str in
  let rec aux lst i =
    if i == len then lst
    else aux (lst @ [str.[i]]) (i + 1)
  in
  aux [] 0

let count_occurences lst a =
  let rec aux l acc =
    match l with
    | [] -> acc
    | hd :: tl when hd == a -> aux tl (acc + 1)
    | hd :: tl -> aux tl acc
  in
  aux lst 0

let unique_counts str =
  let exploded = explode str in
  List.sort_uniq compare exploded
  |> List.map (count_occurences exploded)

let checksum lst =
  let int_of_bool b = if b then 1 else 0 in
  let count_exact h i =
    List.map (fun l -> int_of_bool (List.mem i l)) h
    |> List.fold_left (+) 0 in
  let hashes = List.map unique_counts lst in
  (count_exact hashes 2) * (count_exact hashes 3)

let main () =
  file_to_str_list "input.txt"
  |> checksum
  |> string_of_int
  |> print_endline

let () =
  main()