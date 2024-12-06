let get_char x y str_ls = if x < 0 || y < 0 || x >= String.length (List.nth str_ls 0) || y >= List.length str_ls then '.' else String.get (List.nth str_ls y) x

let set_char x y str_ls c = if x < 0 || y < 0 || x >= String.length (List.nth str_ls 0) || y >= List.length str_ls then str_ls else List.init (List.length str_ls) (fun i -> if i = y then (String.init (String.length (List.nth str_ls i)) (fun j -> if j = x then c else String.get (List.nth str_ls i) j)) else List.nth str_ls i)

let get_patrol_pos str_ls = let rec helper acc = function
        | [] -> raise (Invalid_argument "")
        | x :: xs -> if String.contains x '^' then (String.index x '^', acc) else if String.contains x 'v' then (String.index x 'v', acc) else if String.contains x '<' then (String.index x '<', acc) else if String.contains x '>' then (String.index x '>', acc)
        else helper (acc + 1) xs
in helper 0 str_ls

let rec simulate str_ls = if Bool.not (String.exists (fun c -> c = '^' || c = 'v' || c = '<' || c = '>') (List.fold_left (fun acc a -> acc ^ a) "" str_ls)) then (List.fold_left (fun acc a -> acc ^ a) "" str_ls)
else let (x, y) = get_patrol_pos str_ls
in let dir = get_char x y str_ls
in match dir with
        | '^' -> simulate (if get_char x (y - 1) str_ls <> '#' then set_char x (y - 1) (set_char x y str_ls 'X') '^' else set_char x y str_ls '>')
        | 'v' -> simulate (if get_char x (y + 1) str_ls <> '#' then set_char x (y + 1) (set_char x y str_ls 'X') 'v' else set_char x y str_ls '<')
        | '<' -> simulate (if get_char (x - 1) y str_ls <> '#' then set_char (x - 1) y (set_char x y str_ls 'X') '<' else set_char x y str_ls '^')
        | '>' -> simulate (if get_char (x + 1) y str_ls <> '#' then set_char (x + 1) y (set_char x y str_ls 'X') '>' else set_char x y str_ls 'v')
        | _ -> raise (Invalid_argument "")

let count_x str = String.fold_left (fun acc c -> if c = 'X' then acc + 1 else acc) 0 str

let () = let str = "......................#...#..............#.....................................#...#..............##..............................
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
......................................#.#.........................................#.....#.............................#.........#." in
print_int (count_x (simulate (String.split_on_char '\n' str)))