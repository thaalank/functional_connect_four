type token = Yellow | Red | Empty 

type col = token list

type turn = YellowTurn | RedTurn | RedWins | YellowWins | Tie

type t = col list * turn

exception FullCol

exception InvalidCol 

exception InvalidRow

(** [make_empty_col r] is an Empty token list with [r] Empty tokens, which 
    represents one empty column. *)
let rec make_empty_col = function
  | 0 -> []
  | r -> Empty :: make_empty_col(r - 1) 

(** [make_empty_grid r c] is an Empty grid with [r] rows and [c] columns. 
*)
let rec make_empty_grid r = function
  | 0 -> []
  | c -> make_empty_col r :: make_empty_grid r (c - 1)

let rec make_start_state r c =  
  (make_empty_grid r c, YellowTurn)

(** [get_col c_i cols] is the column at index [c_i] in [cols].
    Raises: InvalidCol if [c_i] is not a valid column index. *)
let get_col c_i cols = 
  try 
    List.nth (cols) c_i 
  with 
  | _ -> raise(InvalidCol)

(** [get_token_row r_i col] is the token at index [r_i] in [col].
    Raises: InvalidRow if [r_i] is not a valid row index. *)
let get_token_row r_i col = 
  try 
    List.nth col r_i 
  with 
  | _ -> raise(InvalidRow)

let get_token c_i r_i t : token =
  get_col c_i (fst(t)) |> get_token_row r_i

let get_turn t : turn =
  snd t 

(** [col_to_list_helper acc col] is a token list representation of [col] but 
    reversed. *)
let rec col_to_list_helper (acc : token list) (col : col) : token list  = 
  match col with 
  | [] -> acc
  | h :: t -> col_to_list_helper (h :: acc) t 

(** [col_to_list acc col] is a token list representation of [col]. *)
let col_to_list (acc : token list) (col : col) : token list = 
  col_to_list_helper acc col |> List.rev

(** [grid_to_list_helper acc grid] is a token list list representation of grid
    but reversed. *)
let rec grid_to_list_helper (acc : token list list) (grid : col list) 
  : token list list = 
  match grid with 
  | [] -> acc
  | h :: t -> grid_to_list_helper (col_to_list [] h :: acc) t

(** [grid_to_list acc grid] is a token list list representation of grid. *)
let grid_to_list (acc : token list list) (grid : col list)
  : token list list = 
  grid_to_list_helper acc grid |> List.rev

let get_board (t : t) : token list list = 
  fst t |> grid_to_list []

(** [consec4 tlist token] is whether the [tlist] has four consecutive [token]. 
*)
let rec consec4 tlst token = 
  match tlst with 
  |[] -> false 
  | h :: t -> 
    if h != token then consec4 t token
    else begin
      match tlst with
      | [] -> false
      | c0 :: c1 :: c2 :: c3 :: t -> 
        if [c0;c1;c2;c3] = [token;token;token;token] then true
        else (consec4 (c1::c2::c3::t) token)
      | _ -> false 
    end

(** [vert4 lst token] is whether [token] satisfies the vertical winning 
    condition in the given [lst] orientation. *)
let rec vert4 (lst: col list) token : bool =
  match lst with 
  | [] -> false 
  | hd :: tl -> 
    if (consec4 hd token) then true 
    else (vert4 tl token)

(** [get_idx col r_i count] gets the desired index of the [col] by matching 
    [r_i] and [count]. *)
let rec get_idx col r_i count =
  if r_i = count then (List.hd col)
  else (get_idx (List.tl col) r_i (count+1))

(** [hori4 lst token] is whether [token] satisfies the horizontal winning 
    condition in the given [lst] orientation. *)
let rec hori4 (lst: col list) token =
  let rec hori4' (lst: col list) i = 
    let get_row lst idx = 
      List.map (fun x -> get_idx x idx 0) lst in 
    if (consec4 (get_row lst i) token) then true
    else if i = ((List.length (List.hd lst)) - 1) 
         && (not (consec4 (get_row lst i) token)) then false
    else hori4' lst (i+1) in (hori4' lst 0)

let rev_all lst = 
  List.map (fun x -> List.rev x) lst

let get_tl lst = 
  List.map (fun x -> List.tl x) lst    

(** [square4x4 lst token] is whether [token] satisfies the winning 
    condition in the given [lst] orientation. *)
let rec square4x4 (lst: col list) token =
  let rec square4x4' lst count = 
    match lst with 
    | [] -> []
    | hd :: tl -> (List.nth hd count) :: (square4x4' tl (count+1)) in 
  let lst_rev = (rev_all lst) in
  if consec4 (square4x4' lst 0) token || 
     consec4 (square4x4' lst_rev 0) token then true 
  else false

(** [square larger lst] is a helper for diag4. Fails with 
    "Error : invalid game board", if the [lst] is empty.  *)
let large_sq (lst : col list) =
  let rec truncate_col lst = 
    match lst with
    | [] -> failwith "Error : invalid game board"
    | hd :: [] -> []
    | hd :: tl -> 
      hd :: (truncate_col tl) in 
  List.map (fun x -> List.tl x) (truncate_col lst)

(** [is_col_full col] is true if the column [col] is full. false if not. *)
let rec is_col_full = function
  | [] -> true
  | Empty :: _ -> false 
  | h :: t -> is_col_full t

(** [check_rect lst] is a helper for diag4. *)
let rec rect (lst : col list) : (col list) =
  let height = List.length lst in
  let width = List.length (List.hd lst) in

  let rec get_by_vertical (lst : col list) (hcounter : int) =
    if hcounter < width then (List.hd lst)::(get_by_vertical (List.tl lst) 
                                               (hcounter + 1)) 
    else [] in

  let rec correct_row (row : token list) (rcounter : int) : token list = 
    if rcounter < height then 
      (List.hd row):: (correct_row (List.tl row) (rcounter + 1))
    else [] in

  let get_by_horizon (lst : col list) rcounter : col list = 
    (List.map (fun x -> (correct_row x rcounter)) lst) in
  if height > width then (get_by_vertical lst 0) 
  else (get_by_horizon lst 0)

(** [diag4 lst token] is whether the player using [token] in [lst] has met the 
    diagonal winning condition. *)
let rec diag4 (lst : col list) token : bool = 
  let cols=lst |> List.length and rows=lst |> List.hd |> List.length in 
  if (rows=4 && cols=4) then (square4x4 lst token) 
  else if ((rows=cols) && (rows>4) && (cols>4)) then 
    diag4 (large_sq lst) token || diag4 (large_sq (List.rev lst)) token ||
    diag4 (large_sq (rev_all lst)) token ||
    diag4 (large_sq (List.rev (rev_all lst))) token
  else if ((abs (rows-cols))=1) then
    if (cols=4) then diag4 (rect lst) token || diag4 (rect (rev_all lst)) token
    else if (rows=4) then 
      diag4 (rect lst) token || diag4 (rect (List.rev lst)) token
    else if (cols>rows) then 
      diag4 (rect lst) token || diag4 (rect (List.rev lst)) token
    else diag4 (rect lst) token || diag4 (rect (rev_all lst)) token
  else if (rows=4) && (cols>4) then 
    diag4 (rect lst) token || (diag4 (List.tl lst) token)
  else if (cols=4) && (rows>4) then 
    diag4 (rect lst) token || (diag4 (get_tl lst)) token
  else diag4 (rect lst) token ||
       (if (cols>rows) then diag4 (rect (List.tl lst)) token
        else diag4 (rect (get_tl lst)) token)

(** [win_condition lst token] is true if the current game [lst] is a win for 
    one of the two players using [token] and false otherwise. *)
let check_win_condition (lst: col list) token = 
  if (vert4 lst token || hori4 lst token || diag4 lst token)
  then true 
  else false

(** [is_col_full col] is true if the column [col] is full. false if not. *)
let rec is_col_full = function
  | [] -> true
  | Empty :: _ -> false 
  | h :: t -> is_col_full t

(** [drop_in_col col token c_i acc] is [col] with [token] dropped into it. *)
let rec drop_in_col col token c_i acc : col =
  match col with 
  | [] -> acc 
  | h:: t -> 
    if h <> Empty then drop_in_col t token c_i (h :: acc)
    else
      t @ [token] @ acc

(** [new_col token c_i grid] is the new column at [c_i] in grid with [token] 
    dropped into it. *)
let new_col token c_i grid = 
  List.rev (drop_in_col (get_col c_i grid) token c_i [])

(** [drop_token token c_i grid acc n] is the new grid with [token] dropped at
    [c_i]. *)
let rec drop_token token c_i grid acc n : col list = 
  match grid with 
  |[] -> acc 
  |h::t -> if c_i = n then acc @ (new_col token c_i grid) :: [] @ t
    else 
      drop_token token (c_i - 1) t (acc @ h :: []) n

(** [next_turn turn] is the next turn if the current turn is [turn]. Fails with 
    "some winning condition/no new turn game is over." if there is no next turn. 
*)
let next_turn = function 
  | YellowTurn -> RedTurn
  | RedTurn -> YellowTurn
  |_ -> 
    failwith "Either a winning condition or a tie has been reached 
    - game is over."

(** [is_grid_full lst] is true if the grid represented by [lst] is full
    of tokens. Otherwise, false.  *)
let rec is_grid_full lst = 
  match lst with 
  | [] -> true 
  | h :: t -> 
    if (is_col_full h) then is_grid_full t 
    else false 

(** [get_winner grid token] is the state associated with the proper winning turn
    based on [token]. *)
let get_winner grid = function 
  | Yellow -> (grid, YellowWins)
  | Red ->(grid, RedWins)
  | _ -> 
    failwith "not a valid token."

let drop token c_i t = 
  match (c_i >= 0) || (c_i <= 6) with
  | true ->    
    let col = (fst t) |> get_col c_i in
    begin
      match is_col_full col with
      | true -> raise FullCol
      | false -> 
        let new_grid = 
          (drop_token token c_i (fst t) [] 0) in 
        if check_win_condition new_grid token then get_winner new_grid token
        else if is_grid_full new_grid then (new_grid,Tie)
        else (new_grid, (next_turn (snd t)))
    end
  | false -> raise(InvalidCol)

(** [get_possible_moves_helper board acc n] is a list of all the possible col_i
    that a token can legally be dropped into [board]. *)
let rec get_possible_moves_helper board acc n = 
  match board with  
  | [] -> List.rev acc 
  | h :: t -> 
    if is_col_full h then get_possible_moves_helper t acc (n + 1) 
    else get_possible_moves_helper t (n :: acc) (n + 1)

let get_possible_moves t = 
  get_possible_moves_helper (fst t) [] 0