
(* ------------------------ *)
(*        listes            *)
(* ------------------------ *)



let len(l : 'a list) : int = List.length l ;;

let fst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error lst : list is empty"
    | hd::[] -> hd
    | _::tail -> lst(tail)
;;

let nth(l, k : 'a list * int) : 'a = 
  let rec nth1(l, k) =
    match l with
      []->  failwith "error nth : index out of bounds"
    | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
  in
    if k < 0
    then failwith "error  nth : index must be positive"
    else nth1(l,k)
;;

let add_fst(l, e : 'a list * 'a) : 'a list = e::l ;;

let rec add_lst(l, e : 'a list * 'a) : 'a list =
  match l with
    [] -> [e]
  | hd::tail -> hd::add_lst(tail,e)
;;

let add_nth(l, e, k  : 'a list * 'a * int) : 'a list =
  let rec add_nth1(l, e, k) =
    match l with
      [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
  in 
    if k < 0
    then failwith "error add_nth : index must be positive"
    else
      if k > len(l)
      then failwith "error add_nth : index out of bounds"
      else add_nth1(l,e,k)
;;

let rem_fst(l : 'a list) : 'a list = 
  match l with
    [] -> failwith "error rem_fst : list is empty"
    | _::tail -> tail
;;

let rec rem_lst(l : 'a list) : 'a list =
  match l with
    [] -> failwith "error rem_lst : list is empty"
    | [x] -> []
    | x::tail -> x::rem_lst(tail)
 ;;

let rem_nth(l, k : 'a list * int) : 'a list =
  let rec rem_nth1(l, k) =
    match l with
    | [] -> failwith "error rem_nth : index out of bounds"
    | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
  in
    if k < 0 
    then failwith "error rem_nth : index must be positive"
    else rem_nth1(l,k)
;;

let concat(l1, l2 : 'a list * 'a list) = l1 @ l2 ;;


(* ------------------------ *)
(*        tableaux          *)
(* ------------------------ *)

let arr_len(t : 'a array) : int = Array.length t ;;

let arr_make(n, v : int * 'a) : 'a array = 
  if n < 0
  then failwith("erreur arr_make ; parametre invalide")
  else Array.make n v 
;;

type 'a matrix = 'a array array ;;

let mat_make(n, m, v : int * int * 'a) : 'a matrix = 
  if n < 0 || m < 0
  then failwith("erreur mat_make ; parametre invalide")
  else Array.make_matrix n m v 
;;


(* ------------------- *)
(*      aleatoire      *)
(* ------------------- *)

let rand_init() : unit = Random.self_init() ;;

let rand_init_expl(n : int) : unit = Random.init(n) ;;

let rand_int_0(n : int) : int = Random.int(n+1) ;;

let rand_int(n, p : int * int) : int = Random.int(p-n + 1) + n ;;


(* ------------------------ *)
(*    lecture caractere     *)
(* ------------------------ *)

let read_char() : char =
  let s : string ref = ref "" and the_end : bool ref = ref false in
    (
    while not(!the_end) 
    do
      s:= read_line() ; 
      if String.length(!s) = 0
      then 
        (
        print_string("erreur read_char : aucun caractere saisi") ;
        print_newline() ;
        )
      else the_end := true;
    done ;
    (!s).[0] ;
    )
;;

(* ------------------------- *)
(* conversion char -> string *)
(* ------------------------- *)

let string_of_char(c : char) : string = Char.escaped c ;;


(* ------------------------ *)
(*    longueur string       *)
(* ------------------------ *)

let string_length(s : string) : int = String.length s ;;


(* ------------------------ *)
(*  pause durant execution  *)
(* ------------------------ *)
(* ------------------------ *)
let wait(n : int) : unit =
  let t : float = Sys.time() and newt : float ref = ref (Sys.time())
    and fn : float = float_of_int(n)
  in
    while (!newt -. t < fn)
    do newt := Sys.time()
    done
;;

