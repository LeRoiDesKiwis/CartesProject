
(* ---------------------------- *)
(*     card_color_of_int        *)
(* ---------------------------- *)

#use "CPutil.ml";;
#use "CP4cards_play.ml"

(* 2 tests pour des valeurs correctes *)

let test_card_color_of_int_a(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_color_of_int_a") in
  let test_result : t_card_color t_test_result = test_exec(test_step, card_color_of_int, 1) in
    (
    assert_true(test_step, "succes 1", test_is_success(test_result));
    test_end(test_step)
    )
;;

let test_card_color_of_int_b(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_color_of_int_b") in
  let test_result : t_card_color t_test_result = test_exec(test_step, card_color_of_int, 3) in
    (
    assert_true(test_step, "succes 3", test_is_success(test_result));
    test_end(test_step)
    )
;;

let test_card_color_of_int_a1(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_color_of_int_a1") in
  let test_result : t_card_color t_test_result = test_exec(test_step, card_color_of_int, 2) in
    (
    assert_true(test_step, "succes 2", test_is_success(test_result));
    test_end(test_step)
    )
;;

let test_card_color_of_int_b1(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_color_of_int_b1") in
  let test_result : t_card_color t_test_result = test_exec(test_step, card_color_of_int, 4) in
    (
    assert_true(test_step, "succes 4", test_is_success(test_result));
    test_end(test_step)
    )
;;

(* 2 tests pour des valeurs erronees *)

let test_card_color_of_int_fail_c(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_color_of_int_fail_c") in
  let test_result : t_card_color t_test_result = test_fail_exec(test_step, card_color_of_int, -3) in
    (
    assert_true(test_step, "succes fail -3", test_is_success(test_result));
    test_end(test_step)
    )
;;

let test_card_color_of_int_fail_d(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_color_of_int_fail_d") in
  let test_result : t_card_color t_test_result = test_fail_exec(test_step, card_color_of_int, 5) in
    (
    assert_true(test_step, "succes fail 5", test_is_success(test_result));
    test_end(test_step)
    )
;;


(* ---------------------------- *)
(*     card_rank_of_int_52      *)
(* ---------------------------- *)

(* 2 tests pour des valeurs correctes *)

let test_card_rank_of_int_52_a(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_rank_of_int_52_a") in
  let test_result : t_card_rank t_test_result = test_exec(test_step, card_rank_of_int_52, 2) in
    (
    if test_is_success(test_result)
    then assert_equals(test_step, "rank_2", test_get(test_result), 2)
    else test_error(test_step) ;
    test_end(test_step)
    )
;;

let test_card_rank_of_int_52_b(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_rank_of_int_52_b") in
  let test_result : t_card_rank t_test_result = test_exec(test_step, card_rank_of_int_52, 11) in
    (
    if test_is_success(test_result)
    then assert_equals(test_step, "rank_11", test_get(test_result), 11)
    else test_error(test_step) ;
    test_end(test_step)
    )
;;


(* 2 tests pour des valeurs erronees *)

let test_card_rank_of_int_52_fail_c(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_rank_of_int_52_fail_c") in
  let test_result : t_card_rank t_test_result = test_fail_exec(test_step, card_rank_of_int_52, -3) in
    (
    assert_true(test_step, "fail -3", test_is_success(test_result));
    test_end(test_step)
    )
;;

let test_card_rank_of_int_52_fail_d(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"card_rank_of_int_52_fail_d") in
  let test_result : t_card_rank t_test_result = test_fail_exec(test_step, card_rank_of_int_52, 15) in
    (
    assert_true(test_step, "fail 15", test_is_success(test_result));
    test_end(test_step)
    )
;;


(* ---------------------------- *)
(*     card_of_int_52           *)
(* ---------------------------- *)


(* 2 tests pour des valeurs erronees *)

let test_card_of_int_52_fail_c(status : t_test_status) : unit = 
  let step : t_test_step = test_start(status,"card_of_int_52_fail_c") in
  let result : t_card t_test_result = test_fail_exec(step, card_of_int_52, 0) in
    (
    if test_is_success(result)
    then assert_equals(step,"fail 0", test_fail_get(result), "erreur card_of_int_52 : parametre invalide") 
    else test_error(step) ;
    test_end(step)
    )
;;

let test_card_of_int_52_fail_d(status : t_test_status) : unit = 
  let step : t_test_step = test_start(status,"card_of_int_52_fail_d") in
  let result : t_card t_test_result = test_fail_exec(step, card_of_int_52, 55) in
    (
    if test_is_success(result)
    then assert_equals(step,"fail 55", test_fail_get(result), "erreur card_of_int_52 : parametre invalide") 
    else test_error(step) ;
    test_end(step)
    )
;;


(* ---------------------------- *)
(*     init_deck_52             *)
(* ---------------------------- *)

(* un test avec quelques verifications d'appartenance de cartes *)
(* ainsi que la verification de la longueur *)

let test_init_deck_52_V1(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"init_deck_52_V1") in
  let test_result : (t_card list) t_test_result = test_exec(test_step, init_deck_52, ()) in
    (
    if test_is_success(test_result)
    then 
      (
      let d52 : t_card list = test_get(test_result) in
        (
        assert_true(test_step, "deck_3", list_contains_value(d52, card_of_int_52(3))) ;
        assert_true(test_step, "deck_16", list_contains_value(d52, card_of_int_52(16))) ;
        assert_true(test_step, "deck_25", list_contains_value(d52, card_of_int_52(25))) ;
        assert_true(test_step, "deck_30", list_contains_value(d52, card_of_int_52(30))) ;
        assert_true(test_step, "deck_44", list_contains_value(d52, card_of_int_52(44))) ;
        assert_true(test_step, "deck_51", list_contains_value(d52, card_of_int_52(51))) ;
        assert_equals(test_step, "longueur", len(d52), 52) ;
        )
      )
    else test_error(test_step) ;
    test_end(test_step)
    )
;;

(* -----------------------------  *)

(* un test combinatoire : verification de l'appartenance de toutes les cartes *)
(* ainsi que la verification de la longueur *)

let test_init_deck_52_V2(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"init_deck_52_V2") in
  let test_result : (t_card list) t_test_result = test_exec(test_step, init_deck_52, ()) in
    (
    if test_is_success(test_result)
    then 
      (
      let d52 : t_card list = test_get(test_result) in
        (
        for i = 1 to 52
        do assert_true(test_step, "deck_"^string_of_int(i), list_contains_value(d52, card_of_int_52(i))) ;
        done ;
        assert_equals(test_step, "longueur", len(d52), 52) ;
        )
      )
    else test_error(test_step) ;
    test_end(test_step)
    )
;;


(* ---------------------------- *)
(*     shuffle                  *)
(* ---------------------------- *)

(* pour chaque test, on verifie que le deck et le deck melange contiennent les memes cartes *)
(* et que les deux sont differents (la proba pour qu'ils soient egaux est tres faible !) *)

let test_shuffle_52(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"shuffle_52") in
  let deck52 : t_card list = init_deck_52() in
  let test_result : (t_card list) t_test_result = test_exec(test_step, shuffle, (deck52, 52)) in
    (
    if test_is_success(test_result)
    then 
      (
      let deck52_shuf : t_card list = test_get(test_result) in
        (
        assert_true(test_step, "similar_52", list_similar_to_list(deck52, deck52_shuf)) ;
        assert_notequals(test_step, "different_52", deck52, deck52_shuf)
        )
      )
    else test_error(test_step) ;
    test_end(test_step)
    )
;;

(* ---------------------------- *)
(*     init_deck                *)
(* ---------------------------- *)

(* 2 tests : configuration a 52 cartes, configuration erronee *)
let test_init_deck_52(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"init_deck_52") and
       p52 : t_param = {cardNb = 52 ; playerNb = 2 ; boardNb = 4 ; cardPerTurnNb = 4 ; turnNb = 6} 
  in
  let test_result : (t_card list) t_test_result = test_exec(test_step, init_deck, p52) in
    (
    if test_is_success(test_result)
    then assert_equals(test_step, "longueur", len(test_get(test_result)), 52)
    else test_error(test_step) ;
    test_end(test_step)
    )
;;


let test_init_deck_fail(status : t_test_status) : unit = 
  let step : t_test_step = test_start(status,"init_deck_fail") and
       p20 : t_param = {cardNb = 20 ; playerNb = 3 ; boardNb = 5 ; cardPerTurnNb = 3 ; turnNb = 3} 
  in
  let test_result : (t_card list) t_test_result = test_fail_exec(step, init_deck, p20) in
    (
    if test_is_success(test_result)
    then assert_true(step,"fail", test_is_success(test_result)) 
    else test_error(step) ;
    test_end(step)
    )
;;


(* ---------------------------- *)
(*     m_player                 *)
(* ---------------------------- *)

(* un test, avec verification du contenu du resultat *)

let test_m_player(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"m_player") and 
       p : t_player ref = ref {id = 3; hand = ref [card_of_int_52(1)] ; cemetery = ref [card_of_int_52(4)]}
  in
  let test_result : t_player t_test_result = test_exec(test_step, m_player, 3) in
    (
    if test_is_success(test_result)
    then 
      (
      p := test_get(test_result) ;
      assert_equals(test_step, "number", (!p).id, 3) ;
      assert_equals(test_step, "hand", !((!p).hand), []) ;
      assert_equals(test_step, "cemetery", !((!p).cemetery), []) ;
      )
    else test_error(test_step) ;
    test_end(test_step)
    )
;;

(* ---------------------------- *)
(*     init_players             *)
(* ---------------------------- *)

(* on se contente d'un test pour une configuration a 4 joueurs *)

let test_init_players(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"init_players") and 
       p52 : t_param = {cardNb = 52 ; playerNb = 4 ; boardNb = 4 ; cardPerTurnNb = 4 ; turnNb = 3}
  in
  let test_result : (t_player array) t_test_result = test_exec(test_step, init_players, p52) in
    (
    if test_is_success(test_result)
    then 
      (
      let t : t_player array = test_get(test_result) in
      let pNB : int = p52.playerNb in
        for i = 0 to 3
        do
          assert_equals(test_step, "number"^string_of_int(i), t.(i).id, i + 1) ;
          assert_equals(test_step, "hand", !(t.(i).hand), []) ;
          assert_equals(test_step, "cemetery", !(t.(i).cemetery), []) ;
          assert_equals(test_step, "len", arr_len(t), pNB);
        done
      )
    else test_error(test_step) ;
    test_end(test_step)
    )
;;


(* ---------------------------- *)
(*       distribute             *)
(* ---------------------------- *)

let test_distribute_structural(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"distribute") in
    let prm : t_param = {cardNb = 52 ; playerNb = 3 ; boardNb = 4 ; cardPerTurnNb = 4 ; turnNb = 3} in
    let players : t_player array = init_players(prm) in
    let deck : t_card list ref = ref [{color = SPADE; rank = 2}; {color = CLUB; rank = 9}] in
	let test_result : (unit) t_test_result = test_exec(test_step, distribute, (players, deck, prm)) in
		(
		if test_is_success(test_result)
		then(
			assert_equals(test_step, "hand_p1", !(players.(0).hand), [{color = SPADE; rank = 2}]) ;
			assert_equals(test_step, "hand_p2", !(players.(1).hand), [{color = CLUB; rank = 9}]) ;
			assert_equals(test_step, "hand_p3", !(players.(2).hand), []) ;
			assert_equals(test_step, "test_deck_2", !deck, []) ;
			)
		else test_error(test_step) ;
		test_end(test_step)
		)
;;

(*
let test_distribute_4cards_functional(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"distribute_4cards") in
	let prm : t_param = {cardNb = 52 ; playerNb = 4 ; boardNb = 4 ; cardPerTurnNb = 4 ; turnNb = 3} in
    let players : t_player array = init_players(prm) in
    let deck : t_card list ref = ref init_deck(prm) in
    let test_result : (unit) t_test_result = test_exec(test_step, distribute_4cards, (players, deck, prm)) in
		(
		if test_is_succes(test_result)
		then(
			for i=0 to 3
			do
				assert_false(test_step, "test_delivery", 
			)
		else test_error(test_step) ;
		test_end(test_step)
		)
;;*)


(* ---------------------------- *)
(*   compute_maxlen_cemetery    *)
(* ---------------------------- *)

let test_compute_maxlen_cemetery_a(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"compute_maxlen_cemetery_a") in
	let p : t_param = {cardNb = 52 ; playerNb = 4 ; boardNb = 4 ; cardPerTurnNb = 4 ; turnNb = 3} in
	let players : t_player array = [|{id = 1; hand = {contents = []}; cemetery = {contents = [{color = SPADE; rank = 2}; {color = CLUB; rank = 9}; {color = CLUB; rank = 7}]}};
									{id = 2; hand = {contents = []}; cemetery = {contents = [{color = HEART; rank = 2};{color = SPADE; rank = 5}]}};
									{id = 3; hand = {contents = []}; cemetery = {contents = [{color = DIAMOND; rank = 7}; {color = SPADE; rank = 8}]}};
									{id = 4; hand = {contents = []}; cemetery = {contents = [{color = DIAMOND; rank = 10};{color = HEART; rank = 11}; {color = HEART; rank = 3};
									{color = HEART; rank = 13}]}}|] in
	let test_result : int t_test_result = test_exec(test_step, compute_maxlen_cemetery, (players, p)) in
		(
		if test_is_success(test_result)
		then(
			assert_equals(test_step, "test_1_max", test_get(test_result), 4);
			)
		else test_error(test_step);
		test_end(test_step)
		)
;;

let test_compute_maxlen_cemetery_b(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"compute_maxlen_cemetery_b") in
	let p : t_param = {cardNb = 52 ; playerNb = 4 ; boardNb = 4 ; cardPerTurnNb = 4 ; turnNb = 3} in
	let players : t_player array = [|{id = 1; hand = {contents = []}; cemetery = {contents = [{color = SPADE; rank = 2}; {color = CLUB; rank = 9}; {color = CLUB; rank = 7}]}};
									{id = 2; hand = {contents = []}; cemetery = {contents = [{color = HEART; rank = 2};{color = SPADE; rank = 5}]}};
									{id = 3; hand = {contents = []}; cemetery = {contents = [{color = DIAMOND; rank = 7}; {color = SPADE; rank = 8}]}};
									{id = 4; hand = {contents = []}; cemetery = {contents = [{color = DIAMOND; rank = 10};{color = HEART; rank = 11}; {color = HEART; rank = 3};]}}|] in
	let test_result : int t_test_result = test_exec(test_step, compute_maxlen_cemetery, (players, p)) in
		(
		if test_is_success(test_result)
		then(
			assert_equals(test_step, "test_2_max", test_get(test_result), 3);
			)
		else test_error(test_step);
		test_end(test_step)
		)
;;

(* ---------------------------- *)
(*     find_index_in_board      *)
(* ---------------------------- *)

let test_find_index_in_board_functional_true(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"find_index_in_board_true") in
    let board : t_card list = [{color = SPADE; rank = 2}; {color = CLUB; rank = 9}; {color = CLUB; rank = 7}; {color = DIAMOND; rank = 10}] in
    let card : t_card = {color = HEART; rank = 7} in
	let test_result : (bool * int) t_test_result = test_exec(test_step, find_index_in_board, (board, card)) in
		(
		if test_is_success(test_result)
		then(
			assert_equals(test_step, "test_true", test_get(test_result), (true, 3));
			)
		else test_error(test_step) ;
		test_end(test_step)
		)
;;

let test_find_index_in_board_functional_false(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"find_index_in_board_false") in
    let board : t_card list = [{color = SPADE; rank = 2}; {color = CLUB; rank = 9}; {color = CLUB; rank = 7}; {color = DIAMOND; rank = 10}] in
    let card : t_card = {color = HEART; rank = 3} in
	let test_result : (bool * int) t_test_result = test_exec(test_step, find_index_in_board, (board, card)) in
		(
		if test_is_success(test_result)
		then(
			assert_equals(test_step, "test_false",test_get(test_result), (false, 0));
			)
		else test_error(test_step) ;
		test_end(test_step)
		)
;;

(* ---------------------------- *)
(*        find_pair             *)
(* ---------------------------- *)

let test_find_pair_functional_true(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"find_pair_true") in
    let board : t_card list = [{color = SPADE; rank = 2}; {color = CLUB; rank = 9}; {color = CLUB; rank = 7}; {color = DIAMOND; rank = 10}] in
    let hand : t_card list = [{color = CLUB; rank = 4}; {color = DIAMOND; rank = 2}; {color = SPADE; rank = 8}; {color = DIAMOND; rank = 1}] in
	let test_result : (bool * int * int) t_test_result = test_exec(test_step, find_pair, (board, hand)) in
		(
		if test_is_success(test_result)
		then(
			assert_equals(test_step, "test_true", test_get(test_result), (true, 1, 2));
			)
		else test_error(test_step) ;
		test_end(test_step)
		)
;;

let test_find_pair_functional_false(status : t_test_status) : unit =
	let test_step : t_test_step = test_start(status,"find_index_in_board_false") in
    let board : t_card list = [{color = SPADE; rank = 2}; {color = CLUB; rank = 9}; {color = CLUB; rank = 7}; {color = DIAMOND; rank = 10}] in
    let hand : t_card list = [{color = CLUB; rank = 4}; {color = DIAMOND; rank = 3}; {color = SPADE; rank = 8}; {color = DIAMOND; rank = 1}] in
	let test_result : (bool * int * int) t_test_result = test_exec(test_step, find_pair, (board, hand)) in
		(
		if test_is_success(test_result)
		then(
			assert_equals(test_step, "test_false", test_get(test_result), (false, 0, 0));
			)
		else test_error(test_step) ;
		test_end(test_step)
		)
;;

(* ---------------------------- *)
(*     fonction de test         *)
(* ---------------------------- *)

let test_run() : unit = 
  let alltests : t_test_status = create_test_status() in
    (
    (* test de card_color_of_int *)
    test_card_color_of_int_a(alltests) ;
    test_card_color_of_int_b(alltests) ;
    test_card_color_of_int_fail_c(alltests) ;
    test_card_color_of_int_fail_d(alltests) ;
    test_card_color_of_int_a1(alltests) ;
    test_card_color_of_int_b1(alltests) ;

    (* test de card_rank_of_int_52 *)
    test_card_rank_of_int_52_a(alltests) ;
    test_card_rank_of_int_52_b(alltests) ;
    test_card_rank_of_int_52_fail_c(alltests) ;
    test_card_rank_of_int_52_fail_d(alltests) ;

    (* test de card_of_int *)
    test_card_of_int_52_fail_c(alltests) ;
    test_card_of_int_52_fail_d(alltests) ;

    (* to be completed with the tests defined above *)
    (* when the corresponding functions are developed *)
    
    (* test de distribute *)
    test_distribute_structural(alltests);
    
    (* test de compute_maxlen_cemetery *)
    test_compute_maxlen_cemetery_a(alltests);
    test_compute_maxlen_cemetery_b(alltests);

	(* test de find_index_in_board *)
	test_find_index_in_board_functional_true(alltests);
	test_find_index_in_board_functional_false(alltests);
	
	(* test de find_pair *)
	test_find_pair_functional_true(alltests);
	test_find_pair_functional_false(alltests);
	
    (* Print test status at the end *)
    print_test_report(alltests)
    )
;;
