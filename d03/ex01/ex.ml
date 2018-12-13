module IntPairs = Set.Make( 
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Pervasives.compare x0 x1 with
        0 -> Pervasives.compare y0 y1
      | c -> c
  end )

let file_to_list filename =
  let ic = open_in filename in
  let extract line x y w h = (line, x, y, w, h) in
  let scan_line line = Scanf.sscanf line "#%d @ %d,%d: %dx%d" extract in
  let rec aux lst =
    try
      aux (scan_line (input_line ic) :: lst)
    with End_of_file -> lst
  in
  let res = aux [] in
  close_in ic ;
  List.rev res

let string_of_pair (a, b) =
  "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"

let string_of_pair_list lst =
  let rec aux l =
    match l with
    | [] -> "]"
    | hd :: tl -> string_of_pair hd ^ " " ^ aux tl
  in
  "[ " ^ aux lst

let generate_pairs (_, xstart, ystart, w, h) : IntPairs.elt list =
  let rec aux x y lst =
    match (x, y) with
    | (_, j) when j = h -> lst
    | (i, j) when i = w -> aux 0 (j + 1) lst
    | (i, j) -> aux (i + 1) j ((xstart + i, ystart + j) :: lst)
  in
  aux 0 0 []


let rec find_multiples lst =
  let rec update_sets (ones, more) l =
    match l with
    | []                                  -> more
    | hd :: tl when IntPairs.mem hd more  -> update_sets (ones, more) tl
    | hd :: tl when IntPairs.mem hd ones  -> update_sets (ones, IntPairs.add hd more) tl
    | hd :: tl                            -> update_sets (IntPairs.add hd ones, more) tl
  in
  update_sets (IntPairs.empty, IntPairs.empty) (List.flatten lst)

let is_not_in_more lst more =
  IntPairs.of_list lst
  |> IntPairs.inter more
  |> IntPairs.cardinal
  |> (=) 0

let rec first_unmatched lst more i =
  match lst with
  | [] -> -1
  | hd :: tl when is_not_in_more hd more -> i
  | hd :: tl -> first_unmatched tl more (i + 1)

let main () =
  let lst = file_to_list "input.txt" in
  let pairs = List.map generate_pairs lst in
  let more = find_multiples pairs in
  first_unmatched pairs more 1
  |> string_of_int
  |> print_endline

let () =
  main()
