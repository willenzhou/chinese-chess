open Piece

type move = Piece.t * (int * int)

(* Create an association list for the current board [pieces] *)
let pieces_to_map pieces= 
  let build accu piece = 
    let pos = get_pos piece in 
    (pos, piece) :: accu in 
  List.fold_left build [] pieces

type result = 
  | Found of Piece.t
  | Not_found

(* Find the piece at the position
   not found if no piece matches the position *)
let piece_at pos pieces = 
  try Found (List.assoc pos pieces)
  with Not_found -> Not_found

(* Check if a piece is out of bound *)
let check_bound pos = 
  let x = fst pos in
  let y = snd pos in
  x >=0 && x <= 9 &&  y >=0 && y <=9

(* Check if a move for han soldier is valid *)
let check_han_soldier x1 y1 x2 y2 = 
  let vert = x2 -x1 in 
  let hori = y2 - y1 in 
  if x1 < 5 then
    (vert = -1 || hori = 1 || hori = -1) && (vert = 0 || hori = 0)
  else
    vert = -1 && hori = 0

(* Check is a move for chu soldier is valid *)
let check_chu_soldier x1 y1 x2 y2 = 
  let vert = x2 -x1 in 
  let hori = y2 - y1 in 
  if x1 > 4 then
    (vert = 1 || hori = 1 || hori = -1) && (vert = 0 || hori = 0)
  else
    vert = 1 && hori = 0

let check_soldier s pos2 = 
  let pos = get_pos s in 
  let x1 = fst pos in 
  let y1 = snd pos in
  let x2 = fst pos2 in 
  let y2 = snd pos2 in
  if get_team s = Han 
  then check_han_soldier x1 y1 x2 y2
  else check_chu_soldier x1 y1 x2 y2

let is_occupied pos pieces = 
  let result = piece_at pos pieces in 
  match result with
  | Found p-> true
  | Not_found -> false

let check_destination pos pieces target = 
  let result = piece_at pos pieces in 
  match result with
  | Found p-> (get_team p) <> (get_team target)
  | Not_found -> true


let increment_in_dir x1 x2 = 
  let diff = x2 - x1 in
  if diff > 0 then x1+ diff -1
  else x1 + diff + 1

let horse_stuck x1 y1 x2 y2 pieces = 
  (* let () = print_int (increment_in_dir x1 x2) in 
     let () = print_int (increment_in_dir y1 y2) in  *)
  let pos1 = (increment_in_dir x1 x2), (increment_in_dir y1 y2) in 
  (is_occupied pos1 pieces)

let check_horse_coords x1 y1 x2 y2 = 
  let xd = x2 - x1 in 
  let yd = y2 - y1 in 
  (xd == 2 && yd == 1) || (xd == 2 && yd == -1) 
  || (xd == -2 && yd == 1) ||  (xd == -2 && yd == -1) 
  || (xd == 1 && yd == 2) ||(xd == 1 && yd == -2) ||
  (xd == -1 && yd == 2) ||(xd == -1 && yd == -2)

let check_horse t pos2 pieces= 
  let pos = get_pos t in 
  let x1 = fst pos in 
  let y1 = snd pos in
  let x2 = fst pos2 in 
  let y2 = snd pos2 in
  (check_horse_coords x1 y1 x2 y2) 
  && not (horse_stuck x1 y1 x2 y2 pieces)

let check_inner_bound pos = 
  let x = fst pos in
  let y = snd pos in
  (x >=0 && x <= 3 &&  y >=3 && y <=5) || 
  (x >=7 && x <= 9 &&  y >=3 && y <=5)

let check_general s pos2 = 
  let pos = get_pos s in 
  let x1 = fst pos in 
  let y1 = snd pos in
  let x2 = fst pos2 in 
  let y2 = snd pos2 in
  let xd = x2 - x1 in 
  let yd = y2 - y1 in 
  (check_inner_bound pos2) &&
  ((xd == 1 && yd == 0) || (xd == 0 && yd == 1) 
   || (xd == -1 && yd == 0) ||  (xd == 0 && yd == -1) )

let check_guard s pos2 = 
  let pos = get_pos s in 
  let x1 = fst pos in 
  let y1 = snd pos in
  let x2 = fst pos2 in 
  let y2 = snd pos2 in
  let xd = x2 - x1 in 
  let yd = y2 - y1 in 
  (check_inner_bound pos2) &&
  ((xd == 1 && yd == 1) || (xd == 1 && yd == -1) 
   || (xd == -1 && yd == 1) ||  (xd == -1 && yd == -1) )

(* Pos of s is guaranteed to on the same line *)
let rec obstruct x1 y1 x2 y2 xaxis pieces=
  match pieces with
  | [] -> false
  | ((x, y), piece) :: t -> 
    begin
      if xaxis 
      then (((x > x1 && x < x2) || (x< x1 && x > x2)) &&  y = y1)
           || (obstruct x1 y1 x2 y2 xaxis t) 
      else ((y > y1 && y < y2) || (y < y1 && y > y2)&&  x = x1)
           || (obstruct x1 y1 x2 y2 xaxis t) 
    end 

let is_clear s pos2 pieces= 
  let pos = get_pos s in 
  let x1 = fst pos in 
  let y1 = snd pos in
  let x2 = fst pos2 in 
  let y2 = snd pos2 in
  if (x1 <> x2) && (y1 <> y2) then false
  else let xaxis = (x1 <> x2) in 
    not (obstruct x1 y1 x2 y2 xaxis pieces)

let count_in_between s pos2 pieces = 
  let rec helper x1 y1 x2 y2 xaxis pieces=
    match pieces with
    | [] -> 0
    | ((x, y), piece) :: t -> 
      begin
        if xaxis 
        then let curr = 
               if ((x > x1 && x < x2) || (x< x1 && x > x2)) && y = y1
               then 1 else 0 in  
          curr +  (helper x1 y1 x2 y2 xaxis t) 
        else let curr = 
               if (y > y1 && y < y2) || (y < y1 && y > y2) && x = x1
               then 1 else 0 in 
          curr +  (helper x1 y1 x2 y2 xaxis t) 
      end 
  in 
  let pos = get_pos s in 
  let x1 = fst pos in 
  let y1 = snd pos in
  let x2 = fst pos2 in 
  let y2 = snd pos2 in
  if (x1 <> x2) && (y1 <> y2) then 0
  else let xaxis = (x1 <> x2) in 
    helper x1 y1 x2 y2 xaxis pieces

let check_chariot s pos2 pieces = 
  is_clear s pos2 pieces

let check_cannon s pos2 pieces = 
  let result = piece_at pos2 pieces in 
  match result with
  | Found p-> 
    (get_team p) <> (get_team s) && 
    (count_in_between s pos2 pieces) = 1
  | Not_found -> is_clear s pos2 pieces

let check_elephant s pos2 pieces = 
  let pos = get_pos s in 
  let x1 = fst pos in 
  let y1 = snd pos in
  let x2 = fst pos2 in 
  let y2 = snd pos2 in
  let diff_x = x2 - x1 in 
  let diff_y = y2 - y1 in 
  (diff_x =2 || diff_x = -2) &&  (diff_y = 2 || diff_y = -2) && 
  not (is_occupied (x1 + diff_x / 2, y1 + diff_y / 2) pieces)

let check_move (t:Piece.t) pos2 board= 
  let pieces = pieces_to_map board in 
  if (check_bound pos2) && (check_destination pos2 pieces t) then
    begin
      match get_genre t with
      | Soldier -> check_soldier t pos2
      | Horse -> check_horse t pos2 pieces
      | Guard -> check_guard t pos2
      | General -> check_general t pos2
      | Chariot -> check_chariot t pos2 pieces
      | Cannon -> check_cannon t pos2 pieces
      | Elephant -> check_elephant t pos2 pieces
    end
  else false

let check_lose board team= 
  let res = List.filter 
      (fun x -> get_genre x = General && get_team x = team) board in 
  List.length res = 0
