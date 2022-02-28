(* -------------------------------------------- *)
(* 1 - Cartes : types et fonction de "creation" *)
(* -------------------------------------------- *)

#use "CPutil.ml";;

type t_card_color = HEART | DIAMOND | CLUB | SPADE ;;

type t_card_rank = int ;;

type t_card = {color : t_card_color ; rank : t_card_rank} ;;

(* fonction rendant une carte a partir d'une couleur et d'un rang *)

let m_card(card_col, card_rank : t_card_color * t_card_rank) : t_card =
  {color = card_col ; rank = card_rank} 

(* Calcul d'un paquet de 52 cartes *)
(* Question 1.1 *)

let card_color_of_int(i : int) : t_card_color =
	if i < 1 || i > 4
	then failwith "Erreur card_color_of_int : valeur hors interval"
	else
		if i = 1
		then HEART
		else
			if i = 2
			then DIAMOND
			else
				if i = 3
				then CLUB
				else SPADE
;;

let card_rank_of_int_52(i : int) : t_card_rank =
	if i < 1 || i > 13
	then failwith "Erreur card_rank_of_int_52 : valeur hors interval"
	else i
;;

let card_of_int_52(i : int) : t_card =
	if i > 52 || i <= 0
	then failwith("erreur card_of_int_52 : parametre invalide")
  else {color = card_color_of_int(((i-1)/13)+1) ; rank = card_rank_of_int_52(((i-1) mod 13)+1)}
;;

(* Question 1.2 *)

let rec init_deck_52_aux(i : int) : t_card list =
	if i = 0
	then []
	else add_lst(init_deck_52_aux(i-1), card_of_int_52(i))
;;

let init_deck_52() : t_card list =
	init_deck_52_aux(52)
;;

(* Mélange d'un paquet de 52 cartes *)
(*Question 1.3*)

let rec shuffle(deck, deck_len : t_card list * int) : t_card list = (
  if deck_len <= 0
  then []
  else (
      let index : int = rand_int(0, deck_len-1) in (
        add_fst(shuffle(rem_nth(deck, index), deck_len-1), nth(deck, index))
    )
  )
);;

(* Paramétrage *)
(* Question 1.4 *)

type t_param = {cardNb : int ; playerNb: int ; boardNb : int ; cardPerTurnNb : int ; turnNb : int};;

let get_cardNb(prm : t_param) : int =
	prm.cardNb;;

let get_playerNb(prm : t_param) : int =
	prm.playerNb;;

let get_boardNb(prm : t_param) : int =
	prm.boardNb;;

let get_cardPerTurnNb(prm : t_param) : int =
	prm.cardPerTurnNb;;

let get_turnNb(prm : t_param) : int =
	prm.turnNb;;

(* Question 1.5 *)
let valid_param(prm : t_param) : bool =
	(get_cardPerTurnNb(prm) * get_playerNb(prm) * get_turnNb(prm) + get_boardNb(prm)) == get_cardNb(prm);;

(* Initialisation du deck *)
(* Question 1.6 *)
let init_deck(prm : t_param) : t_card list =
(
	if get_cardNb(prm) <> 52
	then failwith("Error : Card number is different from 52")
	else (
		if not (valid_param(prm))
		then failwith("Error : parameters aren't valid")
		else shuffle(init_deck_52(), 52)
	)
);;

(* Question 1.8 *)
type t_player = {id: int; hand : t_card list ref; cemetery : t_card list ref};;

let m_player(number : int) : t_player =
{id = number; cemetery = ref [] ; hand = ref []}

(* Réalisation des joueurs *)
(* Question 1.10 *)
let init_players(param : t_param) : t_player array =
	let player_arr = arr_make(param.playerNb, m_player(0)) in
	for i = 0 to param.playerNb -1
	do
		player_arr.(i) <- m_player(i+1)
	done;
	player_arr
;;

(* Distribution des cartes aux joueurs *)
(* Question 2.10 *)

(*variables de tests
let prm = {cardNb = 52; playerNb = 4; boardNb = 4; cardPerTurnNb = 4; turnNb = 3};;
let players : t_player array = init_players(prm);;
let deck : ((t_card list) ref) = ref(init_deck(prm));;*)

let distribute(players, deck, p : t_player array * t_card list ref * t_param) : unit =
	for i = 0 to get_playerNb(p)-1
	do
		if !deck != []
		then( 
			players.(i).hand := add_fst(!(players.(i).hand), fst(!deck));
			deck := rem_fst(!deck);
		)
	done
;;

(* Question 2.13 *)
let distribute_4cards(players, deck, p : t_player array * t_card list ref * t_param) : unit =
	while !deck != []
	do
		distribute(players, deck, p)
	done
;;

(* Question 2.14 *)

let rec fill_board_aux(nb, deck, board : int * t_card list ref * t_card list ref) : unit =
	if nb != 0 && !deck != []
	then 
		board := add_fst(!board, fst(!deck)) ;
		deck := rem_fst(!deck);
		fill_board_aux(nb-1, deck, board)
;;

let fill_board(board, deck, p : t_card list ref * t_card list ref * t_param) : unit =
	fill_board_aux(get_boardNb(p), deck, board)
;;

(* Question 2.16 *)

let compute_maxlen_cemetery(players, p : t_player array * t_param) : int =
	let max : int ref = ref 0 in
	for i=0 to get_playerNb(p) -1
	do
		if len(!(players.(i).cemetery)) > !max
		then max := len(!(players.(i).cemetery))
	done ;
	!max
;;

(* Question 2.17 *)

let compute_winners(players, p : t_player array * t_param) int list =
	let winners : int list ref = ref [] in
	let max : int = compute_maxlen_cemetery(players, p) in
	for i=0 to get_playerNb(p) -1
	do
		if len(!(players.(i).cemetery)) = max
		then winners := add_fst(!winners, i+1)
	done ;
	!winners
;;

let rec find_index_in_board_aux(board, card, i : t_card list * t_card * int) : bool * int = (
    if i >= len(board)
    then false, -1
    else if (nth(board, i)).rank == card.rank
    then true, i
    else find_index_in_board_aux(board, card, i+1);
)

let find_index_in_board(board, card : t_card list * t_card) : bool * int = (
    find_index_in_board_aux(board, card, 0);
)

(* Question 2.19 *)
let same_card_rank(card_1, card_2: t_card * t_card) : bool =
	card_1.rank = card_2.rank
;;

(* Question 2.20 *)

let find_index_in_board(board, card : t_card list * t_card) : bool * int =
	let index : int ref = ref 0 in
	let result : bool ref = ref false in
	let board_bis : t_card list ref = ref board in
	for i=1 to len(board)
	do
		if (fst(!board_bis)).rank = card.rank
		then index := i ;
			result := true ;
			board_bis := rem_fst(!board_bis)
	done ;
	(!result, !index)
;;
