(****************************************)
(* Zadanie:     Sortowanie topologiczne *)
(* Autor:              Konrad Litwinski *)
(* Recenzent:          Szymon Karpiński *)
(****************************************)

exception Cykliczne;;

(*dla wygody jezeli z wierzcholka a jest krawedz do wierzcholka b to bede pisal, 
ze wierzcholek a jest rodzicem b, a b jest dzieckiem a. 
Inaczej niz w przypadku drzew, tutaj moze zachodzic sytuacja ze wierzcholek ma wielu rodzicow*)

(*funkcja do porzadkowania inputu, gdy opisano dzieci danego wierzcholka w wielu roznych tuplach*)
let merge_kids map =
  PMap.foldi (fun x lst0 new_map -> PMap.add x (List.sort_uniq compare lst0) new_map) map PMap.empty

  
(*tworzenie mapy która dla wierzcholka trzyma liste jego dzieci*)
let rec gen_kids_map map = function
    | (x, x_kids) :: tail -> gen_kids_map (PMap.add x (x_kids @ PMap.find x map) map) tail
    | _ -> map


(*zwiekszanie wartosci przypisanej kazdemu z kluczy z listy w mapie o 'addition'*)
let rec add_to_map addition map = function
  | [] -> map
  | x :: tail -> add_to_map addition (PMap.add x (addition + PMap.find x map) map) tail


let topol listN0 =
  (*'all_nodes' to lista wszystkich wierzcholkow*)
  let all_nodes = List.fold_left (fun lst (x, x_kids) -> (x :: x_kids) @ lst) [] listN0 in
  let all_nodes = List.sort_uniq compare all_nodes in

  (*'kids_map' bedzie dla kazdego wierzcholka trzymac liste jego dzieci (rowniez listy puste)*)
  let kids_map = List.fold_left (fun map x -> PMap.add x [] map) PMap.empty all_nodes in
  let kids_map = gen_kids_map kids_map listN0 in
  let kids_map = merge_kids kids_map in

  (*'before_map' dla kazdego wierzcholka trzyma liczbe jego rodzicow (rowniez 0)*)
  let before_map = List.fold_left (fun map x -> PMap.add x 0 map) PMap.empty all_nodes in
  let before_map = PMap.fold (fun kids map -> add_to_map 1 map kids) kids_map before_map in

  (*gen_zero_list bedzie lista wierzcholkow nie posiadajacych rodzica*)
  let gen_zero_list = PMap.foldi (fun k v a -> if v = 0 then k :: a else a) before_map [] in

  (*length to liczba wszystkich wierzcholkow w grafie*)
  let length = PMap.fold (fun _ num -> num + 1) before_map 0 in

  (*final bedzie usuwac ze stosu z grafu (oraz ze stosu 'zero_list') wierzcholek bez rodzica,
  a następnie dodawac na stos 'zero_list' osierocone w ten sposob wierzcholki.
  Lista kolejnych usunietych w ten sposob wierzcholkow jest lista wynikowa*)
  let rec final zero_list map acc =
    match zero_list with
      | [] -> acc
      | x :: tail ->
        let x_kids = PMap.find x kids_map in
        let new_zeros = 
          List.fold_left
          (fun one_list x -> if 1 = PMap.find x map then (x :: one_list) else one_list) 
          [] x_kids
        in
        final (new_zeros @ tail) (add_to_map (-1) map x_kids) (x :: acc)
  in
  
  let result = final gen_zero_list before_map [] in
  (*jezeli final wygeneruje liste dlugosci mniejszej niz 'length' to w grafie istnieje cykl*)
  if List.length result < length then
    raise Cykliczne
  else 
    List.rev result;;


(************************************ TESTY ************************************)

(*

(* pusty graf *)
assert ((topol []) = []) ;;

(* jeden wierzchołek *)
assert (topol [1, []] = [1]) ;;
assert (try ignore(topol [1, [1]]); false with Cykliczne -> true) ;;
assert (try ignore(topol [1, [1; 1]; 1, [1]; 1, [1; 1]]); false with Cykliczne -> true) ;;

(* dwa wierzchołki *)
assert ((topol [1, [2; 2]; 2, []]) = [1; 2]) ;;
assert ((topol [1, [2; 2; 2]; 1, []; 1, [2; 2]; 2, []]) = [1; 2]) ;;
assert (try ignore(topol [1, [1]; 2, [1]]); false with Cykliczne -> true) ;;
assert (try ignore(topol [1, [2]; 2, [1]]); false with Cykliczne -> true) ;;
assert (try ignore(topol [1, [1;2]; 2, [1;2]]); false with Cykliczne -> true) ;;

(* wierzchołek bez wychodzących *)
assert ((List.hd (topol [(1, [2]); (1, [3])])) = 1);;

*)