type base = A | C | G | T | WC (* wildcard *)

type dna = base list

(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)

let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."

(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (str.[i] :: acc) (* str.[i] retourne le i-eme caractere de str et on le concatene a une liste vide*)
  in
  aux (String.length str - 1) [] (*String.length retourne la taille de str.*)

(* conversions *)
let base_of_char (c : char) : base =
  match c with
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _ -> WC

let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s) (*List.map applique la fonction base_of_char a chaque element de la liste (explode s).*)

let string_of_dna (seq : dna) : string =
  String.concat "" (List.map string_of_base seq) (*String.concat concatene les elements de la liste (List.map string_of_base seq) avec "" entre chaque element.*)

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)

(* if list = pre@suf, return Some suf. otherwise, return None *)
let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  let rec aux slice list =
    match slice with
    | [] -> Some (list)
    | x :: sliced -> match list with 
              | [] -> None
              | e :: sl -> 
                if (e == x) then 
                  if sl = [] then Some [] 
                  else aux sliced sl
            else 
              if (sliced == [] && (not (sl = []))) then Some (x::sl) 
            else None
          in aux slice list
;;
(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)

 
 (* je cut prefix si ma liste slice est pas vide alors enlever la premiere valeur de list et rappeler la fonction sur la sous liste avec le meme slice
 si slice vide alors ajouter a after le reste de ma sl*)
let rec before_aux slice list = 
  match slice with
  | [] -> None
  | x :: sliced -> match list with
              | [] -> None
              | e :: sl -> 
                if (x == e) then 
                  if (sliced == []) then Some (sl)
                  else before_aux sliced sl
                else None
;;

let rec check_equal l1 l2 =
  match l1 with
  | [] -> l2 == []
  | e1 :: sl1 -> match l2 with
              | [] -> false
              | e2 :: sl2 -> 
                if (e1 == e2) then check_equal sl1 sl2
                else false
;;

(*
  La fonction before 
  slice : la partie a coupé
  list : la liste sur laquelle on va travailler
  before_tmp : l'accumulateur ou on va stocker notre resultat
  start_list : pour verifier si on obtient pas la meme chose a la fin
  l'idee de la fonction est de recuperer toutes les bases avant le slice
*)
let rec before slice list before_tmp start_list =
  if (list == []) then 
    if (check_equal (List.rev before_tmp) start_list) then [] 
    else List.rev(before_tmp)
  else let res = (before_aux slice list) in 
    if (res == None) then 
      match list with
    | [] -> before_tmp
    | x :: sl -> before slice sl (x::before_tmp) start_list
    else List.rev(before_tmp)
;;

(*
  La fonction after
  slice : la partie a coupe
  list : la liste sur laquelle on va travailler
  l'idee de la fonction est de recuperer toutes les bases apres le slice
*)
let rec after slice list = 
  if (slice == []) then list
  else if (list == []) then []
  else let res = (before_aux slice list) in
  if (res == None) then
    match list with
    | [] -> list
    | x :: sl -> after slice sl
  else Option.get(res)
;;

(* return the prefix and the suffix of the first occurrence of a slice,
    or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  if (list == [] && slice == [])
      then Some(list, slice)
  else
    let before = before slice list [] list in
    let after = after slice list in
    if (before == [] && after == []) then None
  else Some(before, after)
;;

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let get_second_element start =
  match start with
  | Some (_, second) -> Some second
  | None -> None
;;

let get_first_element start =
  match start with
  | Some (first, _) -> Some first
  | None -> None
;;

let reverse_2dlist list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | x :: tl -> aux tl (x :: acc)
  in aux list []
;;

(*
Il faut en gros cree la liste avec la premiere occurence de start puis 
travailler sur cette liste surlaquelle on va appliquer la premiere occurence de stop du coup j'aurai en deuxieme position de start et dans la premiere occurence de stop les elements entre start et stop et je repete jusqu'a que ma liste en vide en rappellant sur le deuxieme argument de stop
*)
let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let rec aux start stop list tmp =
    if (list == []) then tmp else
    let a = first_occ start list in
    if (a != None) then let b = first_occ stop (Option.get(get_second_element a)) in 
      if (b != None) then aux start stop (Option.get(get_second_element b)) (Option.get(get_first_element b) :: tmp)
      else tmp
    else tmp
  in let res = aux start stop list [] in reverse_2dlist res
;;
(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) =
  let start = dna_of_string("ATG") in
  let stop = dna_of_string("TAA") in
  if (String.compare "ATGTAA" (string_of_dna dna) == 0) 
    then [dna_of_string("")] 
  else slices_between start stop dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus


(*auxiliary function to count occurrences of list elements*)
let count_occ (lst : 'a list) : ('a * int) list =
  List.fold_left (fun acc x -> 
    (*Pour chaque x on cherche s'il est dans l'accumulateur avec un nombre d'occurence*)
    match List.assoc_opt x acc with
    | Some count -> (x, count + 1) :: List.remove_assoc x acc (*si trouve, incrementation du compteur*)
    | None -> (x, 1) :: acc (*sinon ajout de l'element avec un compteur initialise a 1*)
    ) [] lst
    
(* return (Full a) if all elements of the list are equal to a,
  (Partial (a, n)) if a is the only element of the list with the
  greatest number of occurrences and this number is equal to n,
  No_consensus otherwise. *)

let consensus (list : 'a list) : 'a consensus =
  match list with
  | [] -> No_consensus (*Liste donc pas de consensus.*)
  | [x] -> Full x (*Consensus complet si un seul element.*)
  | _ -> let lst = count_occ list in
         let max = List.fold_left (fun acc (_, count) -> max acc count) 0 lst in
         (*Trouve le nombre max d'occurrences d'un element.*)
         let lst = List.filter (fun (_, count) -> count = max) lst in
         (*Filtre des elements ayant le nombre max d'occurrences*)
         match lst with
         | [(x, n)] when n = List.length list -> Full x (*Tous les elements egaux donc consensus complet*)
         | [(x, n)] -> Partial (x, n) (*Un element x a le plus grand nombre d'occurrences n donc consensus partiel*)
         | _ -> No_consensus (*Dans tous les autres cas il n'y a pas de consensus*)

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)


(* return the consensus sequence of a list of sequences : for each position
    in the elements of ll, compute the consensus  of the set of values at this
    position  in the sequences. the lists must be of same length. if all lists
    are empty, return the empty sequence.
  *)
let consensus_sequence (ll : 'a list list) : 'a consensus list =
  let rec aux acc i =
    match ll with
    | [] | [] :: _ -> List.rev acc  (*Les listes sont vides ou une d'elles est vide.*)
    | _ ->
      if List.for_all (fun lst -> i < List.length lst) ll then (*Toutes les listes ont un element a l'indice i.*)
        let elements_at_i = List.map (fun lst -> List.nth lst i) ll in (*Recupere les elements a l'indice i.*)
        let consensus_at_i = consensus elements_at_i in (*Calcul du consensus.*)
        aux (consensus_at_i :: acc) (i + 1) (*Appel recursif.*)
      else
        List.rev acc  (*Fin des elements dans les listes.*)
  in
  aux [] 0

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
