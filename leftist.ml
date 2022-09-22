(* Autor rozwiązania: Konrad Litwiński
review przeprowadził Władyysław Pałucki *)

exception Empty;;

(* 'a queue zawiera wariant Null dla pustej kolejki lub wariant Node dla kolejki niepustej 
Node składa się z: priorytetu * skrajnie prawej głębokości poddrzewa * wskaźnika na lewego syna * wskaźnika na prawego syna *)
type 'a queue =
  | Null
  | Node of 'a * int * 'a queue * 'a queue
  
let empty = Null;;


let rec join (tree1: 'a queue) (tree2: 'a queue) =
  match tree1, tree2 with
    | Null, Null -> Null
    | Null, Node (_, _, _, _) -> tree2
    | Node (_, _, _, _), Null -> tree1
    | Node (a1, depth1, tr1L, tr1R), Node (a2, depth2, tr2L, tr2R) -> 
      (
      (* wybieram drzewo z mniejszą liczbą odpowiadającą priorytetowi w korzeniu i zmieniam jego prawe poddrzewo na połączenie go (prawego poddrzewa) z drugim drzewem,
      oznacza to, że funkcja 'join' zostaje ponownie wywołana dostając jako argumenty jedno z dwóch oryginalnyh drzew w całości i co najwyżej połowę drugiego,
      oznacza to, że funkcja 'join' działa w złożoności conajwyżej log2(n) gdzie n to liczba wierzchołków większego drzewa *)
      let (new_a, new_right, new_left) = 
        if a2 < a1 then
          (a2, tr2L, join tr2R tree1)
        else 
          (a1, tr1L, join tr1R tree2)
      in  
      
      (* pozostaje sprawdzić które z poddrzew 'new_right', 'new_left' ma krótszą skrajnie prawą ścieżkę do liścia i przypiąć je 
      jako prawego syna operowanego wierzchołka, a drugie poddrzewo przypiąć jako lewego syna *)
        match new_right, new_left with
          | Null, Null -> Node (new_a, 1, Null, Null)
          | Null, Node (_, depthA, _, _) -> Node (new_a, depthA, new_left, Null)
          | Node (_, depthB, _, _), Null -> Node (new_a, depthB, new_right, Null)
          | Node (_, depthC, _, _), Node (_, depthD, _, _) -> 
            if depthC <= depthD then
              Node (new_a, depthC, new_left, new_right)
            else
              Node (new_a, depthD, new_right, new_left)
      );;
  
(* dodawanie elementu 'a' do kolejki 'tree1' to tak naprawdę łączenie jej z kolejką zawierającą tylko element 'a' *)
let add a (tree1: 'a queue) = join (Node (a, 1, Null, Null)) tree1;;

let delete_min (tree1: 'a queue) =
  match tree1 with
    | Null -> raise Empty
    | Node (a, _, tr1, tr2) -> (a, join tr1 tr2);;

let is_empty (tree1: 'a queue) =
  match tree1 with
    | Null -> true
    | _ -> false;;


(********** TESTY **********)
(*
let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;

let b = join a b ;;
assert (is_empty b <> true);;

let (x,y) = delete_min b;;

assert (x = 1);;
assert (is_empty y = true);;
assert (try let _=delete_min y in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

assert(is_empty b = true);;

let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let (a,b) = delete_min b;;
assert (a = "bxbxc");;

let (a,b) = delete_min b;;
assert (a = "nzbza");;

let (a,b) = delete_min b;;
assert (a = "nzbzad");;

assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;
*)