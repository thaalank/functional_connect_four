open State
open Random

(* size of the board *)
let width = 7
let height = 6

let red_wins = 1000000
let yellow_wins = -red_wins

(** [transpose board] is [board] transposed. *)
let rec transpose board = 
  match board with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

(** [check_horiz_consecs token_lst] is the score of [token_lst] which represents
    a row of the board. *)
let rec check_horiz_consecs = function
  | [] -> 0
  | Yellow::Yellow::Yellow::Empty::_ -> -1000
  | Yellow::Yellow::Empty::Yellow::_ -> -1000
  | Yellow::Empty::Yellow::Yellow::_ -> -1000
  | Empty::Yellow::Yellow::Yellow::Empty::_ -> -2000
  | Empty::Yellow::Yellow::Yellow::_ -> -1000
  | Red::Red::Red::Empty::_ -> 1000
  | Red::Red::Empty::Red::_ -> 1000
  | Red::Empty::Red::Red::_ -> 1000
  | Empty::Red::Red::Red::Empty::_ -> 2000
  | Empty::Red::Red::Red::_ -> 1000
  | Red::Red::Empty::_ -> 100
  | Red::Empty::Red::_ -> 100
  | Empty::Red::Red::_ -> 100
  | Yellow::Yellow::Empty::_ -> -100
  | Yellow::Empty::Yellow::_ -> -100
  | Empty::Yellow::Yellow::_ -> -100
  | _ :: t -> check_horiz_consecs t 

(** [check_vert_consecs token_lst] is the score of [token_lst] which represents
    a column of the board. *)
let rec check_vert_consecs = function
  | [] -> 0
  | Empty :: t -> 0 
  | Red :: Red :: Red :: t -> 1000
  | Red :: Red :: t -> 100
  | Yellow :: Yellow :: Yellow :: t -> -1000
  | Yellow :: Yellow :: t -> -100
  | _ :: t -> check_vert_consecs t 

(** [loop_cols board acc] is the score of each column in [board]. *)
let rec loop_cols board acc = 
  match board with 
  | [] -> acc
  | h :: t -> loop_cols t (check_vert_consecs h + acc)

(** [loop_trans_cols board acc] is the score of each row in [board]. *)
let rec loop_trans_cols board acc = 
  match board with 
  | [] -> acc
  | h :: t -> loop_trans_cols t (check_horiz_consecs h + acc)

(** [score_state t] is the score of a given state [t]. *)
let score_state t = 
  match get_turn t with 
  | RedWins -> red_wins
  | YellowWins -> yellow_wins
  | Tie -> 0
  | _ -> 
    loop_cols (get_board t) 0 + loop_trans_cols (get_board t |> transpose) 0

(** [drop_token t col_i token] is the state after dropping [token] at [col_i] 
    in state [t]. Fails with "not a valid token" if the winning conditions are 
    met but [token] is not a valid token to meet the winning conditions. *)
let drop_token t col_i token = 
  drop token col_i t 

(** [get_possible_moves t] is a list of all the possible col_i that a token
    can legally be dropped into in [t]. *)
let find_possible_moves t = 
  get_possible_moves t  

(** [perform_moves t possible_moves token] is a list of tuples where the first
    element of the tuple is the col_i the token was dropped into, and the second
    element of the tuple is the state after the token is dropped into that 
    col_i. *)
let perform_moves t possible_moves token =
  possible_moves |> List.map (fun col_i -> (col_i, drop_token t col_i token))

(** [flip_token token] is yellow if [token] is red and is red if [token] is 
    yellow. Fails with "Not possible to flip an empty token." if [token] cannot
    be flipped.  *)
let flip_token = function
  | Yellow -> Red
  | Red -> Yellow 
  | _ -> failwith "Not possible to flip an empty token."

(** [lsts_to_tuple_lst lst1 lst2] is [lst1] and [lst2] combined into a list
    of 2-element tuples. *)
let rec lsts_to_tuple_lst lst1 lst2 = 
  match lst1, lst2 with 
  | [], [] -> [] 
  | h :: t, h1 :: t1 -> (h, h1) :: lsts_to_tuple_lst t t1 
  | _ -> failwith "Not possible to convert these lists."

(** [min_max max_min token depth t] is a tuple of the chosen 
    index of column and score if the token is placed at the index of column.
    The min_max algorithm picks the index of column to place the token into 
    where [max_min] indicates whether the algorithm is maximizing or minimizing 
    the score, [token] indicates which token should be dropped at the column 
    index, [depth] indicates the depth of the algorithm, and [t] indicates at 
    which state the token should be dropped. Returns a random column index if 
    the algorithm does not return move. *)
let rec min_max max_min token depth t = 
  match depth with 
  | 0 -> (None, score_state t)
  | _ -> 
    let pos_moves = find_possible_moves t in 
    begin
      match pos_moves with 
      | [] -> (None, score_state t)
      | _ -> 
        let pos_moves_and_states = perform_moves t pos_moves token in 
        find_killer_moves max_min token depth t pos_moves pos_moves_and_states 
    end 

(** [find_killer_moves max_min token depth t pos_moves pos_moves_and_states] 
    is a helper function for [min_max] which is a tuple of the chosen 
    index of column and score if the token is placed at the index of column. *)
and find_killer_moves max_min token depth t pos_moves pos_moves_and_states = 
  let target_score = if max_min then red_wins else yellow_wins in 
  let killer_moves = pos_moves_and_states 
                     |> List.map (fun (col, t) -> (col, score_state t)) 
                     |> List.filter(fun(_, score) -> score = target_score) in 
  match killer_moves with 
  | (killer_move, killer_score) :: t -> 
    (Some(killer_move), killer_score)
  | [] -> let possible_states = List.map (snd) pos_moves_and_states in 
    let best_scores = possible_states 
                      |> List.map (min_max (not max_min) (flip_token token) 
                                     (depth - 1)) 
                      |> List.map snd in 
    let all_scores = lsts_to_tuple_lst pos_moves best_scores in 
    let best  (_,s as l) (_,s' as r) = if s > s' then l else r
    and worst (_,s as l) (_,s' as r) = if s < s' then l else r in
    let best_move, best_score =
      List.fold_left (if max_min then best else worst) (List.hd all_scores) 
        (List.tl all_scores) in (Some(best_move),best_score)

let get_mini_max_c_i max_min token depth t = 
  let best_move, best_score = min_max max_min token depth t in 
  match best_move with 
  | Some(move) -> move 
  | None ->  Random.int(7) 