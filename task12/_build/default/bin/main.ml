let get_char x y str_ls = if x < 0 || y < 0 || x >= String.length (List.nth str_ls 0) || y >= List.length str_ls then '.' else String.get (List.nth str_ls y) x

let set_char x y str_ls c = if x < 0 || y < 0 || x >= String.length (List.nth str_ls 0) || y >= List.length str_ls then str_ls else List.init (List.length str_ls) (fun i -> if i = y then (String.init (String.length (List.nth str_ls i)) (fun j -> if j = x then c else String.get (List.nth str_ls i) j)) else List.nth str_ls i)

let get_patrol_pos str_ls = let rec helper acc = function
        | [] -> raise (Invalid_argument "")
        | x :: xs -> if String.contains x '^' then (String.index x '^', acc) else if String.contains x 'v' then (String.index x 'v', acc) else if String.contains x '<' then (String.index x '<', acc) else if String.contains x '>' then (String.index x '>', acc)
        else helper (acc + 1) xs
in helper 0 str_ls

let rec simulate str_ls ox oy step = if Bool.not (String.exists (fun c -> c = '^' || c = 'v' || c = '<' || c = '>') (List.fold_left (fun acc a -> acc ^ a) "" str_ls)) then false
else let (x, y) = get_patrol_pos str_ls
in if step <> 0 && x = ox && y = oy then true else let dir = get_char x y str_ls
in match dir with
        | '^' -> simulate (if get_char x (y - 1) str_ls <> '#' then set_char x (y - 1) str_ls '^' else set_char x y str_ls '>') ox oy 1
        | 'v' -> simulate (if get_char x (y + 1) str_ls <> '#' then set_char x (y + 1) str_ls 'v' else set_char x y str_ls '<') ox oy 1
        | '<' -> simulate (if get_char (x - 1) y str_ls <> '#' then set_char (x - 1) y str_ls '<' else set_char x y str_ls '^') ox oy 1
        | '>' -> simulate (if get_char (x + 1) y str_ls <> '#' then set_char (x + 1) y str_ls '>' else set_char x y str_ls 'v') ox oy 1
        | _ -> raise (Invalid_argument "")

let () = let str_ls = String.split_on_char '\n' "......................#...#..............#.....................................#...#..............##..............................
......#.....#................#....#...........#........#......................#...............................#.....#.#.###..#...#
...#.#.................#........#.................#.........................................................#.....................
.....................................................................................................#.........#..................
.............................#.................#...................................#.....#....#........................#....#.....
..........................#.....................................................................#..#.............#................
.................#...........................#..........................#..............................#.#..#...................#.
..#....#....#......................................................................#....#.........................................
..................................#............#...........#...........................................#.#....#.............#.#...
...#...##....................................................................................#..........#..................#......
..................#.....#......#.....#...#.........#..#.............................#....##.............#...............##.#..#...
......#...............................................................................................................#..........#
...........#................................#.......#.............#...............#...................................#...........
............##...........................#........................................................................#...............
...............................#........#...#.................................#..#.#.............#...........#....................
........#..........#.#....................................................#........#....#..#.................................#....
.........#..............#........................#................................................................................
..................#..........................#.............#..................#.#...........#.....#...............................
...#...........#.#..........#.....................................................................................................
..............#.................#.....................................................................#...#......#.....#..........
..#.....#................................................................................................#.#.....#..........#.....
#.#..........#...#.............................#...............#..................................................#.#.............
..........................#..#.............#................................................................................#.....
...#..............................................................................................#..............#..#.......#.....
........................................#......#........................................#...................##...........#........
.............#..............#...........................##...#............#.....#..#..............................................
..#....#..#...............#........#.................#............................................#........##...............#.....
................................................#.......#...........##.............................#.##...........................
............................#..............#..#...........#...#.....#.#...................#.....#.............................#...
....................#....................................................................................#........................
.......................................................................................#.......................##..#..............
#.............................#.............................................................#............................#........
..............................................................................#.............#...#.........##......................
...............#..........................#.............#............#...........................................#................
................#........................#................................#...#............#.............................#........
..............................................................#.....#..........#.......#........#....#..................#.........
........................................................#..............................................................#.......#..
..#........#............#........#...#.....#.......#..............................................................................
..#..............#.#.....................................#..................................................#....##...............
............#........................#..........##.................................................................#........#.....
............#............#.#...............................................#.......#..............................................
..................#.........................................#............................#......#..................#...........#..
........................................................#.............##................................#........................#
.....................#...#......#.......#............#........#..................#...#.#....................................#.....
...........................................................................................#..............................#....#..
....#...................#.............#..........#..................................#.............................................
......................................#...............................................#..#........................................
.................#..#............................................#.............................................................#.#
............##...................................................................................#.........................#......
.........#.................................................#..#........................................#.....#..........#.....#...
.........#........................................................................................#.#...............#............#
..........#..#...........#...................#.....#.......#......#....#..#.......................#..........#..........#.........
......#...................................................#....#..............#....#.........##...#...............................
.........#........#........................#...........................................................#.......#................#.
.....................#...............................................#.##.........................................................
................................................................................#.............#..............#............#.......
......#..........#............................................#...#....................#...............................#.........#
......................................#....#.........#..............................................................#...#.........
.......##...#.#..........................................................#............#...........#........#...................#..
.....#............................................................................#............................#..................
....#..................#....#......#......#......................#......................#.......................................#.
..#..............#..#............#.................................#............#..................#................#.............
........................................................#....#...................................##.......................#.......
..........#..................................#....#........#.......#..........................#................#...............#..
.....................................#.................#..................#...........#...........................................
.........#..............#...................................................................................#...................#.
.................##.................................#.......#.......................................................#.........#...
...........#.................#..........................#...............................#..#...........................#..........
.....................................#...........................#...................#..............................#..#..........
.............................#.....#.................................................#.................##...............#.......#.
............#.........................................................................................................#...........
..........##.............#...................#.#...........................................#....................................##
..........#..#...#...............#........#........................................................#.................#............
....................#......................#....................................................#.#.....................#.........
.......................................#............#........................##............#....#..#..............................
..........#..........#.....................................#.................................#.......#............................
....#.............#...............................................................................................................
..#..............................................#....#......#........................................#.................#.........
...#......#.....#..............................#....#....................#......#..................#...................#.......#..
...............#..#...........................##..................................................................................
........#..#.....#.........................#.......#.............#..........................................#................#....
...#......#.............#...............#.............................................................................##..........
#.................................#.......#.........#.#.#...................................#...#.#...........#..........#........
#.........#....#...#.........#.#.............................................................#....................................
........................#.#.......#.................................................#.............................................
...#...##...#..#...........................#.....#..........................#...............................#.......#.............
................#..............................#..........##...................................................................#..
.......................................................................#...#..#...#.....#....#..................##..........#.....
#.....................#.........#........#.....................................................................#.....#.......#....
...........#......................#.......#.................................................#........................#............
.......#.........................................#.....#...#...........#................#....#......#...........#......#..........
....#.............#...............................#...#....#..#..#...........................................#.............#......
...#.........#..........................................#.....#...........^.#...#....#...........................#................
.....#..#...............#...........................#.#..........#........................................................#.......
.............#.......#.............................................................................................#...#..........
..#...................#...............................#......#............#...........#.......#...................................
...........#.......................#..........#...#.......................#..............#.............##.............#..#........
..#............#.............#.......................................................#.....##....................................#
.#.........#.....................................................................#.........#......................................
.............#....................................................................#.....#.............#...#...........#..........#
..........................#.....#...#......................#.................................#.....#.................#.......#....
..........#.#.....................................................................................................................
............#.......#....................#.......#.............................................#..................................
.......................................#..#........#...................................................#..........................
...................#........#....#........#..................#..........................................................#.........
........#..............#....................#............#......#...........#.....#..............................#.........#......
.......#.....#.................#.#..#..............................................................#.......#..#...........#......#
.#....................##.....................................................#...#.....#..........................................
...........#..................#.....#....#............#...............#....#............#.......................#.................
........#.....................................#......##............#..........................#..........#....................#...
..........................#...............................................................#.......................................
......................#..................................#.....................#.............................................#...#
.........#..............................................................#........##.........#.......#.............................
....#...#........#...........#....#.............#............#.....#..............................................................
...........#...............................................................................................#.......#..#...........
........#..........................#............................#...........#............#.............................#..........
.......#...........................#...#...............#..#..#...................#.........#......................................
..#..............................#........#...........#.......................................#...#...............................
.............#...#.............#.........#...........#.....................................#................#.....................
.........................#..........................#.................................#...........................................
..................##.............#........#......#............#........................#.......#......................#....#......
.....#............................................................................................................................
.#..................................#.....#.............#...#.............................................#..............#..##..#.
...............#........................................................#.........#..#..........#.........................#.......
.............................................#...................#.................#..................#......................#....
............................#.......................................................................................#.............
............#...............#....................#....#..........#..#..............................#......#.......#.......##......
............#.................................................................................#...................................
........................#...............................................................#......#.......#.......#.......###......#.
......................................#.#.........................................#.....#.............................#.........#."
in let (x, y) = get_patrol_pos str_ls
in print_int (List.fold_left (fun acc a -> if simulate a x y 0 then acc + 1 else acc) 0 (List.map (fun (i, j) -> set_char i j str_ls '#') (List.filter (fun (i, j) -> get_char i j str_ls = '.') (List.flatten (List.init (List.length str_ls) (fun j -> (List.init (String.length (List.nth str_ls j)) (fun i -> (i, j)))))))))
