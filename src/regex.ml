open Regex_base

(*
  J'utilise n comme iterateur et des que j'ai concatener assez de fois la liste l je la renvoie   
*)
let repeat n l =
  let rec aux n l tmp =
    if (n > 0) then (aux (n - 1) l (tmp @ l))
    else tmp
  in aux n l []
;;


(*
  Meme chose mais cette fois ci avec un mots et je stock ma valeur dans acc
*)
let rec expr_repeat n e =
  let rec aux n e acc =
    if n <= 0 then
      acc
    else
      aux (n - 1) e (Concat (e, acc))
    in aux n e Eps
;;

let rec is_empty e =
  match e with
  | Eps -> true
  | Joker -> false
  | Base e -> false
  | Star e1 -> is_empty e1
  | Concat(e1, e2) -> is_empty e1 && is_empty e2
  | Alt(e1, e2) -> is_empty e1 && is_empty e2
;;

let rec null e =
  match e with
  | Eps -> true
  | Joker -> false
  | Base e -> false
  | Star e -> true
  | Concat(e1, e2) -> null e1 && null e2
  | Alt(e1, e2) -> null e1 || null e2
;;

let rec is_finite_Star e = 
  match e with
  | Eps -> true
  | Joker -> false
  | Base e -> false
  | Star e1 -> is_finite_Star e1
  | Concat(e1, e2) -> is_finite_Star e1 && is_finite_Star e2
  | Alt(e1, e2) -> is_finite_Star e1 && is_finite_Star e2
;;

(*
  Mon mot est pas fini tant que je n'ai pas de "Star" et des que j'ai une etoile dans mon expression je verfie juste si c'est que des mot vide (is_finite_Star a la ligne 39) sinon cela veux dire que l'expression n'est pas fini
*)
let rec is_finite e =
    match e with
    | Eps -> true
    | Joker -> true
    | Base e -> true
    | Star e1 -> is_finite_Star e1
    | Concat(e1, e2) -> is_finite e1 && is_finite e2
    | Alt(e1, e2) -> is_finite e1 && is_finite e2
;;

let product l1 l2 =
  List.concat (List.map (fun x -> List.map (fun y -> x @ y) l2) l1)
  (*
    List.map (fun y -> x @ y) l2 : pour un element x de l1,
    on produit une liste de toute les concatenations possibles de x
    avec les elements de l2

    List.map (fun x -> ... ) l1 : on applique a chaque element de l1
    la fonction ci-dessus
    
    List.concat : applatit la liste de liste en une liste
  *)
  
let rec enumerate alphabet expr =
  match expr with
  | Eps -> Some [[]] 
  | Base c -> if List.mem c alphabet then Some [[c]]  (* si c est dans alphabet, on renvoie Some [[c]] *)
              else None
  | Joker -> Some (List.map (fun c -> [c]) alphabet) (* on renvoie Some [[a]; [b]; [c]; ...] *)
  | Star _ -> None (* on ne peut pas enumerer une etoile *)
  | Alt (e1, e2) ->
      (
      match (enumerate alphabet e1, enumerate alphabet e2) with (* on enumere e1 et e2 *)
      | (Some l1, Some l2) -> Some (l1 @ l2) (* on concatene les deux listes *)
      | _ -> None (* si l'une des deux listes est None, on renvoie None *)
      )
  | Concat (e1, e2) ->
      (
      match (enumerate alphabet e1, enumerate alphabet e2) with (* on enumere e1 et e2 *)
      | (Some l1, Some l2) -> Some (product l1 l2) (* on fait le produit cartesien des deux listes *)
      | _ -> None (* si l'une des deux listes est None, on renvoie None *)
      )
;;

let compare s1 s2 = 
  compare s1 s2
;;

(*
  Je verifie a chaque fois si chaque la lettre est contenu dans la liste tmp
  (ligne 106 avec List.mem)
  et je l'ajoute dedans si elle n'est pas encore dedant   
*)
let alphabet_expr e =
  let rec aux e tmp = 
    match e with
    | Eps -> tmp
    | Base e1 ->  if ((List.mem e1 tmp) == false) then (e1::tmp) else tmp
    | Joker -> tmp
    | Star e1 -> aux e1 tmp
    | Alt(e1, e2) -> aux e1 (aux e2 tmp)
    | Concat(e1, e2) -> aux e1 (aux e2 tmp)
  in List.sort compare (aux e [])
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
    
type answer =
  Infinite | Accept | Reject

(*
  1) Je verifie si tout d'abord si l'expression e est infini
  Sinon
    a) je fais un cas précis pour Epsilon a la ligne 137 pour accepter la chaine vide
    b) sinon je cree l'aphabet du resultat du mot e avec w
    Et je verifie si le mot w est dans cette alphabet
*)
let accept_partial e w =
  if (is_finite e == false) then 
    Infinite
  else
    let rec is_accept list w = 
      match list with
      | [] -> Reject
      | x :: sl -> if (check_equal x w) then Accept else is_accept sl w 
    in let a = (alphabet_expr e) @ w
    in let l = enumerate a e in
    if (l == None) then
      is_accept [] w 
    else
      is_accept (Option.get(l)) w
;;
