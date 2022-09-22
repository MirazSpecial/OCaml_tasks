type point = float * float;;

type kartka = point -> int;;

(* poniższy typ reprezentuje prostą skierowaną z first_point do second_point*)
type half_line = point * point;;

(* dla uniknięcia błędów zaokrąglania floatow *)
let eps = 1e-9;;

(* zwraca wektor z pierwszego punktu do drugiego *)
let points_to_vector (x1, y1) (x2, y2) = 
  (x2 -. x1, y2 -. y1);;

(* zwraca obraz podanego punktu po przesunięciu o podany wektor *)
let point_plus_vector ((x1, y1): point) (x2, y2) = 
  (x1 +. x2, y1 +. y2);; 

(* sprawdzamy czy pierwszy punkt leży na prostokącie tworzonym przez pozostałe dwa, zakładam, że x1 <= x2 oraz y1 <= y2 *)
let is_in_rectangle ((x, y): point) (x1, y1) (x2, y2) = 
  (x +. eps >= x1 && x <= x2 +. eps && y +. eps >= y1 && y <= y2 +. eps);;

(* sprawdzamy czy pierwszy punkt leży na kole o środku (x1, y1) i promieniu r, zakładam, że r >= 0 *)
let is_in_circle ((x, y): point) (x1, y1) r = 
  hypot (x -. x1) (y -. y1) <= r +. eps;;

(* right_or_left dla danego punktu i prostej skierowanej (half_line) zwraca 0 jeśli punkt leży na tej prostej,
1 jeżeli leży po jej prawej stronie i -1 jeżeli leży po jej lewej stronie, za pomocą iloczynu wektorowego *)
let right_or_left ((x, y): point) ((point1, point2): half_line) =
  let (vecA1, vecA2) = points_to_vector point1 point2 in
  let (vecB1, vecB2) = points_to_vector point2 (x, y) in
  let res = (vecA1 *. vecB2) -. (vecA2 *. vecB1) in
  if res > eps then 1
  else if res < -.eps then -1
  else 0;;

(* symmetrical dla danego punktu i prostej zwraca obraz tego punktu z symetrii względem tej prostej.*)
let symmetrical ((x, y): point) (point1, point2) =
  let (vecA1, vecA2) = points_to_vector point1 point2 in
  let (vecB1, vecB2) = points_to_vector point2 (x, y) in
  let vec1_size = vecA1 ** 2.0 +. vecA2 ** 2.0 in
  let scalar = ((vecA1 *. vecB1) +. (vecA2 *. vecB2)) in
  let new_vector = (scalar *. vecA1 /. vec1_size, scalar *. vecA2 /. vec1_size) in
  let (sym1, sym2) = point_plus_vector point2 new_vector in
  (* (sym1, sym2) to rzut punktu (x, y) na daną w funkcji prostą, więc należy zwrócić obraz (x, y) w symetrri
  środkowej względem (sym1, sym2) *)
  (2. *. sym1 -. x, 2. *. sym2 -. y);;

(* prostokat zgodnie z trescia zadania zwraca funkcje która dla danego punktu zwraca liczbę przebić prostokątnej kartki 
o wierzchołkach podanych w 'prostokat' przy wbiciu szpilki w dany punkt *)
let prostokat (x1, y1) (x2, y2) =
  let construct pin = 
    if is_in_rectangle pin (x1, y1) (x2, y2) then 1
    else 0
  in
  construct;;

(* kolko zgodnie z trescia zadania zwraca funkcje która dla danego punktu zwraca liczbę przebić okrągłej kartki
o środku i promieniu podanym z 'kolko' przy wbiciu szpilki w podany punkt*)
let kolko (x1, y1) r =
  let construct pin =
    if is_in_circle pin (x1, y1) r then 1
    else 0
  in
  construct;;

(* zloz zgodnie z trescią zadania zwraca funkcje która dla danego punktu zwraca liczbę przebić odpowiednio złożonej kartki
przy wbiciu szpilki w dany punkt*)
let zloz point1 point2 page =
  let construct pin =
    let pin_prim = symmetrical pin (point1, point2) in
    let holes = right_or_left pin (point1, point2) in
    if holes = 1 then 
      page pin + page pin_prim
    else if holes = 0 then
      page pin
    else 0
  in
  construct;;

(* zloz_do_folda to odpowiednik fnkcji zloz dostosowany do wbudowanej procedury wyższego rzędu - fold_left*)
let zloz_do_folda page (point1, point2) = zloz point1 point2 page;;

(* skladaj zgodnie z trescią zadania zwraca funkcje która dla danego punktu zwraca liczbę przebić odpowiednio poskładanej kartki
przy wbiciu szpilki w dany punkt*)
let skladaj lines_lst page =
  List.fold_left zloz_do_folda page lines_lst;;


(*let a = prostokat (0., 0.) (10., 10.);;

assert(a (0., 0.) = 1);;
assert(a (5., 5.) = 1);;
assert(a (10., 10.) = 1);;
assert(a (10., 0.) = 1);;
assert(a (0., 10.) = 1);;
assert(a (10.1, 0.) = 0);;
assert(a (0., 10.1) = 0);;
assert(a (10.1, 10.1) = 0);;

let a = zloz (5., 0.) (5., 377.) a;;

assert(a (0., 0.) = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

let a = zloz (5., 0.) (5., 1.) a;;

assert(a (0., 0.) = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;*)