type tree = Leaf of int list | Node of tree list * int list

let scan_input filename =
  let ic = open_in filename in
  let line = input_line ic in
  close_in ic ;
  line
  |> Str.split (Str.regexp " +")
  |> List.map int_of_string

let string_of_string_list lst =
  let rec aux l =
    match l with
    | [] -> " ]"
    | hd :: tl -> hd ^ " " ^ aux tl
  in
  "[ " ^ aux lst

let string_of_int_list lst =
  List.map string_of_int lst
  |> string_of_string_list

let split_at_nth lst n =
  let rec aux tuple i =
    match tuple with
    | _ when i = n -> tuple
    | (left, hd :: tl) -> aux (left @ [hd], tl) (i + 1)
    | _ -> tuple
  in aux ([], lst) 0

let rec create_tree children lst  =
  match lst with
  | c :: m :: tl when c = 0 -> let metadata, remaining = split_at_nth tl m in Leaf(metadata), remaining
  | c :: m :: tl when c = List.length children ->
    let metadata, remaining = split_at_nth tl m
    in Node(children, metadata), remaining
  | c :: m :: tl ->
    let grandchildren, l = create_tree [] tl
    in create_tree (children @ [grandchildren]) (c :: m :: l)
  | _ -> raise (Failure "invalid tree")

let rec sum_metadata tree =
  let sum_list l = List.fold_left (+) 0 l in
  match tree with
  | Leaf(l) -> sum_list l
  | Node(c, m) -> List.map sum_metadata c
                  |> sum_list
                  |> (+) (sum_list m)

let main () =
  let input = scan_input "input.txt" in
  let tree, _ = create_tree [] input in
  sum_metadata tree
  |> string_of_int
  |> print_endline

let () =
  main()
