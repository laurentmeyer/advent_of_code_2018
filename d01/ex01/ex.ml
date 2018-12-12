module IntSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = int
  end )

let file_to_int_list filename =
  let ic = open_in filename in
  let rec aux lst =
    try
      let l = int_of_string (input_line ic) in
      aux (lst @ [l])
    with End_of_file -> begin close_in ic ; lst end
  in
  aux []

let main () =
  let input = file_to_int_list "input.txt" in
  let rec aux values last history =
    match values with
    | [] -> aux input last history
    | hd :: tl when IntSet.mem (hd + last) history -> hd + last
    | hd :: tl -> let n = hd + last in aux tl n (IntSet.add n history)
  in
  aux input 0 (IntSet.empty |> IntSet.add 0)
  |> string_of_int
  |> print_endline

let () =
  main ()