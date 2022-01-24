#use "CPutil.ml";;

type t_card_color = HEART | DIAMOND | CLUB | SPADE;;
type t_card_rank = int;;
type t_card = {color : t_card_color ; rank : t_card_rank};;

let m_card(card_col, card_rank : t_card_color * t_card_rank) : t_card =
{color = card_col ; rank = card_rank}
;;

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

(*Question 2*)

let rec init_deck_52_aux(i : int) : t_card list=
  (
    if i = 0
    then []
    else add_fst(init_deck_52_aux(i-1), card_of_int_52(i))

  )

let rec init_deck_52() : t_card list =
init_deck_52_aux(52)