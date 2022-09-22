(* pierwszy int to początek przedziału, drugi to jego koniec, we wszystkich przedziałach zakładamy że początek przedziału 
  jest niewiększy od końca *)
type interval =
  int * int;;

(* Node składa się z: lewego poddrzewa, przedziału w danym wierzchołku, prawego poddrzewa,
  głębokości danego drzewa, oraz minimum z ilości liczb w danym poddrzewie i max_int 
  lewe poddrzewo zawiera przedziały z liczbami mniejszymi niż liczby z przedziału w wierzchołku, 
  a prawe poddrzewa przedzały z liczbami iększymi *)  
type t =
  | Empty
  | Node of t * interval * t * int * int;;

(* span zwraca ilość liczb całkowitych z przedziału <x1, x2> lub max_int jeżeli liczb tych jest więcej niż max_int
  zakładam tu, że x1 <= x2 *)
let span x1 x2 = 
  if x2 - x1 + 1 <= 0 then 
    max_int
  else
    x2 - x1 + 1;;

(* height_f zwraca wysokość danego drzewa *)
let height_f t0 =
  match t0 with
    | Empty -> 0
    | Node (_, _, _, res, _) -> res;;

(* weight_f zwraca liczbę liczb całkowitych we wszystkich przedziałach w wierzchołkach tego poddrzewa, 
  lub max_int jeśli liczba ta przekracza max_int *)
let weight_f t0 =
  match t0 with
    | Empty -> 0
    | Node (_, _, _, _, res) -> res;;

(* new_height_f zwraca wysokość drzewa powstałego przez połączenie t1 t2 pod tym damym wierzchołkiem *)
let new_height_f t1 t2 = 1 + max (height_f t1) (height_f t2);;

(* new_weight_f zwraca liczbę liczb całkowitych we wszystkich wierzchołkach drzewa stworzonego przez połączenie 
dwóch poddrzew pod jednym wierzchołkiem *)
let new_weight_f (x1, x2) t1 t2 = 
  let new_w = span x1 x2 + weight_f t1 + weight_f t2 in
  if new_w <= 0 then
    max_int
  else 
    new_w;;

(* make zwraca drzewo powstałe po połączeniu dwórch poddrzew (t1, t2) pod jednym wierzchołkiem *)
let make t1 (x1, x2) t2 = 
  Node (t1, (x1, x2), t2, new_height_f t1 t2, new_weight_f (x1, x2) t1 t2);;

(* intersection dla dwóch przedziałów zwraca true jeśli przedziały można połączyć (zgodnie z zasadami podanymi w poleceniu),
  lub false jeśli nie można. Działa również gdy zachaczamy o max_int i min_int *)
let intersection (a1, a2) (b1, b2) =
  if a2 = b2 then
    true
  else if a2 > b2 then
    if b2 + 1 >= a1 then true
    else false
  else 
    if a2 + 1 >= b1 then true
    else false;;

(* correct jest odpowiednikiem funkcji bal z pSet, l to lewe poddrzewo (x1, x2) to przedział w korzeniu, a r to prawe poddrzewo.
  Jeżeli wysokość któregoś z drzew l lub r jest większa od wysokości drugiego o więcej niż 2 to przepniemy korzeń tak,
  żeby różnica ta została zredukowana o jeden *)
let correct l (x1, x2) r =
  let hl = height_f l in
  let hr = height_f r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height_f ll >= height_f lr then make ll lk (make lr (x1, x2) r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr (x1, x2) r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height_f rr >= height_f rl then make (make l (x1, x2) rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l (x1, x2) rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, (x1, x2), r, new_height_f l r, new_weight_f (x1, x2) l r)

(* place dodaje wierzchołek z przedziałem (x1, x2) do drzewa t0, przy założeniu że dla każdego przedziału (y1, y2) w drzewie t0 
  zachodzi intersection (x1, x2) (y1, y2) = 0 *)
let rec place (x1, x2) t0 =
  match t0 with
    | Empty -> make Empty (x1, x2) Empty
    | Node (l_kid, (y1, y2), r_kid, _, _) ->
      if x1 > y2 then
        correct l_kid (y1, y2) (place (x1, x2) r_kid)
      else
        correct (place (x1, x2) l_kid) (y1, y2) r_kid;;

(* join łączy dwa zbilansowane drzewa l_kid i r_kid przy czym zakładamy, że maksymalna wartość w którymkolwiek wierzchołku l_kid
  jest mniejsza niż minimalna wartość w którymkolwiek wierzchołku r_kid 
  funkcja działa w złożoności liniowej po wysokości wyższego drzewa czyli w złożoności logarytmicznej po jego wielkości *)
let rec join l_kid r_kid =
  match (l_kid, r_kid) with
    | Empty, Empty -> Empty
    | Empty, _ -> r_kid
    | _, Empty -> l_kid
    | Node (ll_kid, (x1, x2), lr_kid, height_l, _), Node (rl_kid, (y1, y2), rr_kid, height_r, _) ->
      if height_l <= height_r then
        let new_l = join l_kid rl_kid in
        correct new_l (y1, y2) rr_kid
      else
        let new_r = join lr_kid r_kid in
        correct ll_kid (x1, x2) new_r;;

(* power_join bilansuje drzewo którego korzeń i dwójkę zbilansowanych dzieci podajemy, robi to w złożoności liniowej po różnicy w wysokości dzieci,
odpowiednik funkcji join z psetu (działa analogicznie) *)
let rec power_join l_kid (x1, x2) r_kid =
  match (l_kid, r_kid) with
    | (Empty, Empty) -> make Empty (x1, x2) Empty
    | (Empty, _) -> place (x1, x2) r_kid
    | (_, Empty) -> place (x1, x2) l_kid
    | (Node (ll_kid, (a1, a2), lr_kid, height_l, _), Node (rl_kid, (b1, b2), rr_kid, height_r, _)) ->
      if height_l > height_r + 2 then
        correct ll_kid (a1, a2) (power_join lr_kid (x1, x2) r_kid)
      else if height_r > height_l + 2 then
        correct (power_join l_kid (x1, x2) rl_kid) (b1, b2) rr_kid
      else 
        make l_kid (x1, x2) r_kid

(* go_left usuwa z danego poddrzewa liczby większe niż x (przy czym x może się zmniejszyć jeżeli istnieje w poddrzewie przedział <x1, x2>
  taki, że x1<x i przedziały <x1, x2> i <x, max_int> można połączyć, wtedy x stanie się x1). go_left redukuje liczbę wierzchołków danego drzewa,
  a ponieważ schodzi cały czas w dół oryginalnego drzewa to ma złożoność liniową po wysokości drzewa.
  Funkcja zwraca zmodyfikowane drzewo *)
let rec go_left x t0 =
  match t0 with 
    | Empty -> (x, Empty)
    | Node (l_kid, (x1, x2), r_kid, _, _) ->
      if intersection (x, max_int) (x1, x2) then
        let (new_x , new_me) = go_left (min x x1) l_kid in
        (new_x, new_me)
      else
        let (new_x, new_right) = go_left x r_kid in
        (new_x, power_join l_kid (x1, x2) new_right);;

(* analogicznie do go_left, fo_right usuwa z danego poddrzewa liczby mniejsze niż x (przy czym x może się zwiększyć jeżeli 
  istnieje w poddrzewie przedział <x1, x2> taki, że x<x2 i przedziały <x1, x2> i <x, max_int> można połączyć, wtedy x stanie się x2). 
  go_left redukuje liczbę wierzchołków danego drzewa, a ponieważ schodzi cały czas w dół oryginalnego drzewa 
  to ma złożoność liniową po wysokości drzewa. Funkcja zwraca zmodyfikowane drzewo *)
let rec go_right x t0 =
  match t0 with
    | Empty -> (x, Empty)
    | Node (l_kid, (x1, x2), r_kid, _, _) ->
      if intersection (min_int, x) (x1, x2) then
        let (new_x, new_me) = go_right (max x x2) r_kid in
        (new_x, new_me)
      else
        let (new_x, new_left) = go_right x l_kid in
        (new_x, power_join new_left (x1, x2) r_kid);;
(* w obu powyższych przypadkach ponieważ użyta została funkcja power_join to zwracane drzewa są zbilansowane*)


(* deconstruct tostaje przedział (x1, x2) oraz pewne poddrzewo którego korzeń zostanie rozszerzony przez dodanie (x1, x2)
  ma zwrócić to poddrzewo po dodaniu do niego (x1, x2) czyli po poscalaniu przedziałów które da się zcaalić i rozszerzeniu przedziału
  z korzenia. Funkcja ta odpala go_left i go_right oraz join więc działa w złożoności logarytmicznej po wielkości poddrzew lefty i righty*)
let deconstruct (x1, x2) (lefty, (y1, y2), righty) = 
  let (new1, new_lefty) = go_left (min x1 y1) lefty in
  let (new2, new_righty) = go_right (max x2 y2) righty in
  power_join new_lefty (new1, new2) new_righty
  (* (zgodnie z działaniem funkcji go_left i go_right) new1 równa się x1 lub jest od niego mniejsze jeżeli przediał (x1, x2) można przedłużyć w dół
    natomiast new2 równa się x2 lub jest od niego większe jeżeli przedział (x1, x2) można przedłużyć w górę *)
  (*join (correct new_lefty (new1, new2) new_righty) Empty*)

(* remove_within dostaje wierzchołek drzewa zawierający przedział (y1, y2) do którego należy przedział (x1, x2) który chcemy usunąć 
  wiadomo więc, że y1 <= x1 oraz y2 >=x2. Po 'zredukowaniu' przedziału (y1, y2) poprzez usunięcie z niego przedziału (x1, x2).
  remove_within może więc usunąc cały wierzchołek (wtedy trzeba zwrócić join l_kid r_kid), może skrócić przedział w wierzchołku, lub 
  *)
let remove_within (l_kid, (y1, y2), r_kid) (x1, x2) =
  if y1 = x1 && y2 == x2 then
    join l_kid r_kid
    (* tu łączymy dwa zbilansowane drzewa l_kid i r_kid*)
  else if y1 = x1 then
    make l_kid (x2 + 1, y2) r_kid 
  else if y2 = x2 then
    make l_kid (y1, x1 - 1) r_kid
  else
    (* tu mamy do połączenie drzewa l_kid r_kid i drzewa składające się z jednego wierzchołka zawierającego przedziały (y1, x1 - 1) i (x2 + 1, y2)*)
    let new_me1 = place (y1, x1 - 1) l_kid in
    let new_me2 = place (x2 + 1, y2) r_kid in
    join new_me1 new_me2


let empty = 
  Empty;;

let is_empty t0 = 
  t0 = Empty;;

(* add dostaje przedział (x1, x2) i drzewo t0 do którego ma dodać ten przedział, add znajduje poddrzewo które zostanie
  jakkolwiek zmienione po dodaniu (x1, x2) i odpala na nim funkcję deconstruct lub jeśli take pooddrzewo nie istnieje to 
  tworzy wierzchołek zawierający dodawany przedział. Działa w złożoności logarytmicznej po wielkości t0*)  
let rec add (x1, x2) t0 =
  match t0 with
  | Empty -> Node (Empty, (x1, x2), Empty, 1, span x1 x2) 
  | Node (l_kid, (y1, y2), r_kid, depth, weight) ->
    if intersection (x1, x2) (y1, y2) then
      deconstruct (x1, x2) (l_kid, (y1, y2), r_kid)
    else if x1 > y2 then
      let r_kid_recon = add (x1, x2) r_kid in
      power_join l_kid (y1, y2) r_kid_recon
    else 
      let l_kid_recon = add (x1, x2) l_kid in
      power_join l_kid_recon (y1, y2) r_kid;;

(* remove doda najpierw przedział x1 x2 do drzewa, przez co wiemy, że potem będzie istnieć wierzchołek z przedziałem <y1, y2> taki,
  że x1>=y1 i x2<=y2 czyli tak na prawdę będziemy usuwać przedział <x1 x2> ze środka jednego przedziału *)
let remove (x1, x2) t0 =
  let t1 = add (x1, x2) t0 in
  (* loop znajdzie wierzchołek do którego należy (x1, x2) przy czym wiem, że taki wierzchołek istnieje
    i odpali na nim funkcję remove_within *)
  let rec loop (x1, x2) tt =
    match tt with 
      | Empty -> assert false
      | Node (l_kid, (y1, y2), r_kid, _, _) ->
        if x1 >= y1 && x2 <= y2 then
          remove_within (l_kid, (y1, y2), r_kid) (x1, x2)
        else if x2 < y1 then
          correct (loop (x1, x2) l_kid) (y1, y2) r_kid
        else
          correct l_kid (y1, y2) (loop (x1, x2) r_kid)
  in
  loop (x1, x2) t1;;

let elements t0 =
  (* loop przechodzi po drzewie in-order i generuje listę zawartości kolejnych wierzchołków *)
  let rec loop t0 lst0 =
    match t0 with 
      | Empty -> lst0
      | Node (l_kid, (x1, x2), r_kid, _, _) ->
        let new_lst = loop l_kid lst0 in
        let new_lst = (x1, x2) :: new_lst in
        let new_lst = loop r_kid new_lst in
        new_lst
  in
  List.rev (loop t0 []);;

(* jako, że każdy wierzchołek trzyma ilość liczb naturalnych we wszystkich przedziałach swojego poddrzewa,
  to w funkcji below wystarczy przejść po odpowiednich wierzchołkach i zsumować te wartości *)
let rec below n t0 =
  match t0 with
    | Empty -> 0
    | Node (kid_l, (x1, x2), kid_r, _, weight) ->
      if x1 > n then
        below n kid_l
      else 
        let new_w = (weight_f kid_l) + (span x1 (min x2 n)) + below n kid_r in
        if new_w <= 0 then
          max_int
        else 
          new_w;;

(* standardowe przejście po drzewie *)
let rec mem n t0 =
  match t0 with
    | Empty -> false
    | Node (kid_l, (x1, x2), kid_r, _, _) ->
      if n < x1 then 
        mem n kid_l
      else if n > x2 then
        mem n kid_r
      else
        true;;

(* standardowe przejście po drzewie, kolejność wywoływania f dla wierzchołków - in-order *)
let rec iter (f:int * int -> unit) t0 =
  match  t0 with
    | Empty -> ()
    | Node (kid_l, (x1, x2), kid_r, depth, weight) ->
      iter f kid_l;
      f (x1, x2);
      iter f kid_r;;

(* standardowe przejście po drzewie *)
let rec fold f t0 a =
  match t0 with
    | Empty -> a
    | Node (kid_l, (x1, x2), kid_r, _, _) ->
      let modified_a = fold f kid_l a in
      let modified_a = f (x1, x2) modified_a in
      let modified_a = fold f kid_r modified_a in
      modified_a;;

(* funkcja split używa funkcji mem żeby stwierdzić czy n jest w t0, a zwraca t1 i t2 usuwając odpowiednio elementy niemniejsze
  niż n w t0 i elementy niewiększe niż n w t0*)
let split n t0 =
  let is_it_in = mem n t0 in
  let t1 = remove (n, max_int) t0 in
  let t2 = remove (min_int, n) t0 in
  (t1, is_it_in, t2);;


  (*let a = add (0, 5) empty;;
  let a = add (7, 8) a;;
  let a = add (-3, -3) a;;
  let a = add (10, 13) a;;
  assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
  assert(below 8 a = 9);;
  let b = add (6, 6) a;;
  let b = remove (6, 6) b;;
  let b = add (-100, -5) b;;
  let b = add (-4, 6) b;;
  assert(elements b = [(-100, 8); (10, 13)]);;
  assert(below 10 b = 110);;
  let c = remove (2, 10) a;;
  assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
  assert(below 12 c = 5);;*)