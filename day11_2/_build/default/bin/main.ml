let parse str = List.map (String.trim) (String.split_on_char ' ' str)

let rm_leading_zer str = String.fold_left (fun acc a -> if acc = "" && a = '0' then acc else acc ^ (Char.escaped a)) "" str

let blink stone_ls =
  let rec helper acc ls = match ls with
  | [] -> acc
  | stone :: ls -> if stone = "0" then helper ("1" :: acc) (ls)
  else if String.length stone mod 2 = 0 
    then helper ((String.sub stone 0 (String.length stone / 2)) :: (rm_leading_zer (String.sub stone (String.length stone / 2) (String.length stone / 2))) :: acc) ls
  else helper ((string_of_int (int_of_string stone * 2024)) :: acc) ls
in helper [] stone_ls

let iter f n = String.fold_left (fun acc _ -> fun x -> f (acc x)) (f) (String.make (n - 1) ' ')

let () = let str = "4022724 951333 0 21633 5857 97 702 6"
in print_int (List.length ((iter (blink) 75) (parse str))); print_newline ()