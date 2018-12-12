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

let diff_count lst1 lst2 =
  let rec aux a b acc =
    match (a, b) with
    | ([], [])                                -> acc
    | (hda :: tla, hdb :: tlb) when hda != hdb -> aux tla tlb (acc + 1)
    | (hda :: tla, hdb :: tlb)                -> aux tla tlb acc
    | _                                       -> acc
    in
    aux lst1 lst2 0

let diff_str str1 str2 =
  diff_count (explode str1) (explode str2)

let rec matchbox str lst =
    match lst with
  | [] -> None
  | hd :: tl when diff_str str hd = 1 -> Some (str, hd)
  | hd :: tl -> matchbox str tl

let print_common_str str1 str2 =
  let rec aux a b =
    match (a, b) with
    | ([], [])                                -> ()
    | (hda :: tla, hdb :: tlb) when hda = hdb -> begin print_char hda ; aux tla tlb end
    | (hda :: tla, hdb :: tlb)                -> aux tla tlb
    | _                                       -> ()
    in
    aux (explode str1) (explode str2)


let main () =
  let lst = file_to_str_list "example.txt" in
  let rec aux l =
    match l with
    | [] -> print_endline "nul"
    | hd :: tl -> match matchbox hd tl with
        | Some (a, b) -> print_common_str a b
        | None -> aux tl
  in aux lst

let () =
  main ()