open Move
open Piece

let rec random_piece lst team= 
  let piece = let len = List.length lst in 
    let r = Random.int len in 
    List.nth lst r in 
  if get_team piece = team then piece
  else random_piece lst team

let all_viable_pos piece board = 
  let res = ref [] in
  let x = ref 9 in 
  let y = ref 9 in
  let () = while !x >=0 do 
      y := 9;
      while !y >= 0 do
        (if check_move piece (!x, !y) board 
         then res := (!x, !y) :: (!res)
         else ());
        y := !y - 1
      done;
      x := !x-1
    done in 
  !res

let random_pos piece board = 
  let potentials = all_viable_pos piece board in
  let len = List.length potentials in 
  if len = 0 then None else
    let r = Random.int len in 
    Some (List.nth potentials r)

let rec dummy_ai board team = 
  let piece_to_move = random_piece board team in 
  let res = random_pos piece_to_move board in 
  match res with
  | None -> dummy_ai board team
  | Some (x, y) -> piece_to_move, (x, y)

(* Prepare for the Minimax algorithm *)

(** [generate_valid_moves board team] is the random 10 moves a 
    computer can make. We keep the number at 10 to reduce the time and deepen
    the layer in minimax algorithms. *)
let generate_valid_moves board team = 
  let rec build curr num = 
    if num = 0 then curr
    else let new_move = dummy_ai board team in 
      new_move :: curr
  in build [] 10

let execute_move board (piece, pos2) = 
  let new_piece = 
    piece_of_tuple (get_genre piece, get_team piece, pos2) in 
  new_piece::List.filter (fun x -> get_pos x <> pos2) board

let possible_new_states board moves = 
  List.map (fun x -> execute_move board x) moves

let initial_score = 0

let can_eat p1 p2 board= 
  let pos2 = get_pos p2 in
  check_move p1 pos2 board

let get_val piece = 
  match get_genre piece with
  | Soldier -> 2
  | Horse -> 4
  | Guard -> 3
  | General -> 100
  | Chariot -> 10
  | Cannon -> 8
  | Elephant -> 2

let one_pass_deduction board init piece= 
  let check init p2 = 
    let pos2 = get_pos p2 in 
    if get_team piece = get_team p2 then init
    else 
      begin
        if check_move piece pos2 board 
        then -(get_val piece) + init
        else init
      end
  in List.fold_left check init board

let threats_points team board init= 
  let deduct init x= 
    if get_team x = team 
    then one_pass_deduction board init x
    else 0 in 
  List.fold_left deduct init board

let rem_val_points team board init= 
  List.fold_left 
    (fun acc x -> if get_team x = team 
      then acc + get_val x else acc) 
    init board

let offense_bonus board init = 
  let check_offense piece = 
    let team = get_team piece in
    let x = fst (get_pos piece) in 
    (team = Chu && x < 5) || (team = Han && x > 5)
  in List.fold_left 
    (fun acc x -> if check_offense x then acc + 2 else 0)
    init board

let eval board team = 
  initial_score
  |> threats_points team board
  |> rem_val_points team board
  |> offense_bonus board

let smarter_random board team = 
  let moves = generate_valid_moves board team in
  let nexts = possible_new_states board moves in
  let max curr move state  = 
    if eval state team > eval (execute_move board curr) team
    then curr
    else move
  in List.fold_left2 max (List.hd moves) moves nexts

let resolve_layer board team is_min = 
  let moves = generate_valid_moves board team in
  let nexts = possible_new_states board moves in
  let max curr move state  = 
    if eval state team> eval (execute_move board curr) team
    then 
      begin
        if is_min then move else curr
      end
    else
      begin
        if is_min then curr else move
      end
  in List.fold_left2 max (List.hd moves) moves nexts

let other_team team = 
  match team with
  | Chu -> Han
  | Han -> Chu

let rec minimax board team is_min depth stored_move= 
  let res = ref 0 in
  match depth with
  | 4 -> eval board team
  | _ -> 
    let moves = generate_valid_moves board team in
    let nexts = possible_new_states board moves in
    let max (curr: move) (move: move) state  = 
      let other = (minimax state (other_team team) 
                     (not is_min)  (depth + 1) stored_move) in 
      let curr_val = (minimax (execute_move board curr)
                        (other_team team) (not is_min) (depth + 1) stored_move) 
      in if other > curr_val
      then 
        begin
          if is_min then (res:= curr_val; curr)
          else (res:= other; move)
        end
      else
        begin
          if is_min then (res:= other; move) 
          else (res:= curr_val; curr)
        end
    in (stored_move := List.fold_left2 max (List.hd moves) moves nexts); !res

let minimax_ai board team= 
  let stored_move = ref (List.hd board, (0, 0)) in 
  let _ = minimax board team false 0 stored_move in
  !stored_move
