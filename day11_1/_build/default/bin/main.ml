let parse str = List.map (String.trim) (String.split_on_char ' ' str)

let to_string	ls = (List.fold_left (fun acc a -> if acc = "[" then acc ^ a else acc ^ "; " ^ a) "[" ls) ^ "]"

let rec blink stone_ls = match stone_ls with
  | [] -> []
  | stone :: stone_ls -> if stone = "0" then "1" :: (blink stone_ls)
  else if String.length stone mod 2 = 0 
    then (String.sub stone 0 (String.length stone / 2)) :: (string_of_int (int_of_string (String.sub stone (String.length stone / 2) (String.length stone / 2)))) :: (blink stone_ls)
  else (string_of_int (int_of_string stone * 2024)) :: (blink stone_ls)

let iter f n = String.fold_left (fun acc _ -> fun x -> let res = acc x in print_endline (to_string res); f res) (f) (String.make (n - 1) ' ')

let () = let str = "4022724 951333 0 21633 5857 97 702 6"
in print_int (List.length ((iter (blink) 25) (parse str))); print_newline ()
