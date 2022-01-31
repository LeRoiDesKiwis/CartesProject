(* -------------------------------------------- *)
(* 1 - Cartes : types et fonction de "creation" *)
(* -------------------------------------------- *)

#use "APutil.ml";;

type t_card_color = HEART | DIAMOND | CLUB | SPADE ;;

type t_card_rank = int ;;

type t_card = {color : t_card_color ; rank : t_card_rank} ;;

(* fonction rendant une carte a partir d'une couleur et d'un rang *)

let m_card(card_col, card_rank : t_card_color * t_card_rank) : t_card =
  {color = card_col ; rank = card_rank} 

(* Calcul d'un paquet de 52 cartes *)
(* Question 1 *)

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
	{color = card_color_of_int(((i-1)/13)+1) ; rank = card_rank_of_int_52(((i-1) mod 13)+1)}
;;

(* Question 2 *)

let rec init_deck_52_aux(i : int) : t_card list =
	if i = 0
	then []
	else add_lst(init_deck_52_aux(i-1), card_of_int_52(i))
;;

let init_deck_52() : t_card list =
	init_deck_52_aux(52)
;;

(* Mélange d'un paquet de 52 cartes *)
(*Question 3*)

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
(* Question 4 *)

type t_param = {cardNB : int ; playerNB : int ; boardNB : int ; cardPerTurnNB : int ; turnNB : int};;

(* Question 4 *)
let get_cardNB(prm : t_param) : int =
	prm.cardNB;;

let get_playerNB(prm : t_param) : int =
	prm.playerNB;;

let get_boardNB(prm : t_param) : int =
	prm.boardNB;;

let get_cardPerTurnNB(prm : t_param) : int =
	prm.cardPerTurnNB;;

let get_turnNB(prm : t_param) : int =
	prm.turnNB;;

(* Question 5 *)
let valid_param(prm : t_param) : bool =
	(get_cardPerTurnNB(prm) * get_playerNB(prm) * get_turnNB(prm) + get_boardNB(prm)) == get_cardNB(prm);;

(* Question 6 *)
let init_deck(prm : t_param) : t_card list =
(
	if get_cardNB(prm) <> 52
	then failwith("Error : Card number is different from 52")
	else (
		if not (valid_param(prm))
		then failwith("Error : parameters aren't valid")
		else shuffle(init_deck_52(), 52)
	)
);;

(* Question 8 *)
type t_player = {id: int; hand : t_card list ref; cemetery : t_card list ref};;
let m_player(number : int) : t_player =
{id: number; cemetery = [] ; hand = []}