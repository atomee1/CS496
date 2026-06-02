(*
  William Ee
  CS 496-A
  Due 2/9/2025
  I pledge my honor that I have abided by the Stevens Honor System.
*)

type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1]

let mirror_image (p: program) : program =
  List.map (function  (* Mirrors all movements *)
    | 2 -> 4  (* Turns move north into south *)
    | 4 -> 2  (* Turns move south into north *)
    | 3 -> 5  (* Turns move east into west *)
    | 5 -> 3  (* Turns move west into east *)
    | x -> x  (* No change for pen up or pen down *)
  ) p

let rotate_90_letter (p: program) : program =
  List.map (function  (* Rotates all movements 90 degrees *)
    | 2 -> 3  (* Turns move north into east *)
    | 3 -> 4  (* Turns move east into south *)
    | 4 -> 5  (* Turns move south into west *)
    | 5 -> 2  (* Turns move west into north *)
    | x -> x  (* No change for pen up or pen down *)
  ) p

let rotate_90_word (pl: program list) : program list =
  List.map rotate_90_letter pl  (* Simply maps rotate_90_letter function to all letters in list *)

let rec repeat (n: int) (x: 'a) : 'a list =
  if n <= 0 then []  (* If n is 0 or < 0, returns empty list *)
  else x :: repeat (n - 1) x  (* Prepends x then recurses n - 1 times *)

let pantograph (n: int) (p: program) : program =
  List.flatten (List.map (function x ->
    if x = 0 || x = 1 then [x]  (* No change for pen up or pen down *)
    else repeat n x) p)  (* Repeats movements n times *)

let rec pantograph_nm (n: int) (p: program) : program =
  match p with
  | [] -> []
  | x :: xl ->
    (if x = 0 || x = 1 then [x]  (* No change for pen up or pen down *)
    else repeat n x) @ pantograph_nm n xl  (* Repeats movements n times *)

let pantograph_f (n: int) (p: program) : program =
  List.fold_right (fun x acc ->
    (if x = 0 || x = 1 then [x]  (* No change for pen up or pen down *)
    else repeat n x) @ acc) p []  (* Repeats movements n times *)

let coverage ((x, y): int * int) (p: program) : (int * int) list =
  let move (x, y) = function  (* Makes coordinates based on instruction *)
    | 2 -> (x, y + 1)  (* Move north *)
    | 3 -> (x + 1, y)  (* Move east *)
    | 4 -> (x, y - 1)  (* Move south *)
    | 5 -> (x - 1, y)  (* Move west *)
    | _ -> (x, y)  (* No change for pen up or pen down *)
  in
  let rec help pos p acc =
    match p with
    | [] -> List.rev acc  (* Reverses to keep movement order *)
    | h :: t -> let new_pos = move pos h in help new_pos t (new_pos :: acc)  (* Tracks new position then recurses *)
  in (x, y) :: help (x, y) p []

let compress (p: program) : (int * int) list =
  let rec help count acc = function
    | [] -> List.rev acc  (* Reverses to keep movement order *)
    | [x] -> List.rev ((x, count + 1) :: acc)  (* Handles last element *)
    | x :: (y :: _ as xl) ->
      if x = y then help (count + 1) acc xl  (* Increases count if instruction repeats *)
      else help 0 ((x, count + 1) :: acc) xl  (* Stores count then resets for next instruction *)
  in help 0 [] p

let rec uncompress (p: (int * int) list) : program =
  match p with
  | [] -> []
  | (x, n) :: xl -> repeat n x @ uncompress xl  (* Expands instruction then recurses *)

let uncompress_m (p: (int * int) list) : program =
  List.flatten (List.map (fun (x, n) -> repeat n x) p)  (* Maps expansion to each instruction *)

let uncompress_f (p: (int * int) list) : program =
  List.fold_right (fun (x, n) acc -> repeat n x @ acc) p []  (* Expands instructions and accumulates *)

let optimize (p: program) : program =
  let rec help last_pen acc = function
    | [] -> List.rev acc  (* Reverses to keep movement order *)
    | 1 :: xl when last_pen = Some 1 -> help (Some 1) acc xl  (* Ignores repeated pen up instructions *)
    | 0 :: xl when last_pen = Some 0 -> help (Some 0) acc xl  (* Ignores repeated pen down instructions *)
    | x :: xl -> help (Some x) (x :: acc) xl  (* Keeps unique instructions *)
  in help (Some 1) [] p  (* Assumes pen starts up *)
