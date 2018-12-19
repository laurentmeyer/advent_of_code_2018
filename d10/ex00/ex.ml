let string_of_string_list lst =
  let rec aux l =
    match l with
    | [] -> "]"
    | hd :: tl -> hd ^ " " ^ aux tl
  in
  "[ " ^ aux lst

let string_of_tuple (a, b, c, d) =
  [a ; b ; c ; d]
  |> List.map string_of_int
  |> string_of_string_list

let string_of_int_list lst =
  List.map string_of_int lst
  |> string_of_string_list

let generate_tuple line =
  Scanf.sscanf line "position=< %d,  %d> velocity=< %d,  %d>" (fun a b c d -> (a, b, c, d))

let generate_list filename =
  let ic = open_in filename in
  let rec aux l =
    try
      begin
        input_line ic
        |> generate_tuple
        |> fun x -> x :: l
        |> aux
      end
    with End_of_file -> close_in ic ; List.rev l
  in aux []

let update_tuple (x, y, a, b) = (x + a, y + b, a, b)

let size_of_representation lst =
  let x_list = List.map (fun (x, _, _, _) -> x) lst in
  let y_list = List.map (fun (_, y, _, _) -> y) lst in
  let spread l = List.fold_left max min_int l - List.fold_left min max_int l in
  (spread x_list, spread y_list)

let represent_message lst =
  let min_x =
    List.map (fun (x, _, _, _) -> x) lst
    |> List.fold_left min max_int in
  let min_y =
    List.map (fun (_, y, _, _) -> y) lst
    |> List.fold_left min max_int in
  let new_list = List.map (fun (x, y, _, _) -> (x - min_x, y - min_y)) lst in
  let line_count = 
    List.map (fun (_, y) -> y) new_list
    |> List.fold_left max min_int
    |> (+) 1 in
  let col_count = 
    List.map (fun (x, _) -> x) new_list
    |> List.fold_left max min_int
    |> (+) 2 in
  let bytestring = Bytes.make (line_count * col_count) '.' in
  List.init line_count (fun i -> (i + 1) * col_count - 1)
  |> List.iter (fun i -> Bytes.set bytestring i '\n') ;
  List.map (fun (x, y) -> y * col_count + x) new_list
  |> List.iter (fun i -> Bytes.set bytestring i '#') ;
  Bytes.to_string bytestring

let find_smallest_representation lst =
  let rec aux i l (prevx, prevy) =
    match size_of_representation l with
    | (x, y) when x > prevx || y > prevy -> i - 1
    | (x, y)       -> aux (i + 1) (List.map update_tuple l) (x, y)
  in
  aux 0 lst (size_of_representation lst)

let rec iterate_n_times lst n =
  if n = 0 then lst else
  iterate_n_times (List.map update_tuple lst) (n - 1)

  let main () =
    let lst = generate_list "input.txt" in
    find_smallest_representation lst
    |> iterate_n_times lst
    |> represent_message
    |> print_endline

let () =
  main()
