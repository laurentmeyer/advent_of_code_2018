module IntPairs = Set.Make( 
  struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
           | c -> c
       end )

let string_of_pair (a, b) =
  "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"

let string_of_pair_list lst =
  let rec aux l =
    match l with
    | [] -> "]"
    | hd :: tl -> string_of_pair hd ^ " " ^ aux tl
    in
    "[ " ^ aux lst

let generate_pairs line : IntPairs.elt list =
  let f xstart ystart w h =
    let rec aux x y lst =
      match (x, y) with
      | (_, j) when j = h -> lst
      | (i, j) when i = w -> aux 0 (j + 1) lst
      | (i, j) -> aux (i + 1) j ((xstart + i, ystart + j) :: lst)
    in
    aux 0 0 []
  in
  Scanf.sscanf line "#%_d @ %d,%d: %dx%d" f

let rec update_sets (ones, more) lst =
  match lst with
  | []                                  -> (ones, more)
  | hd :: tl when IntPairs.mem hd more  -> update_sets (ones, more) tl
  | hd :: tl when IntPairs.mem hd ones  -> update_sets (ones, IntPairs.add hd more) tl
  | hd :: tl                            -> update_sets (IntPairs.add hd ones, more) tl


let main () =
  let ic = open_in "input.txt" in
  let rec aux (ones, more) =
    try
      begin
        input_line ic
        |> generate_pairs
        |> update_sets (ones, more)
        |> aux
      end
    with End_of_file -> more
  in
  aux (IntPairs.empty, IntPairs.empty)
  |> IntPairs.cardinal
  |> string_of_int
  |> print_endline
  ;
  close_in ic


let () =
  main()
