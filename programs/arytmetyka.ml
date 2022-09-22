    (* Poniższy typ danych zawiera wariant 'Non' dla wartości nieokreślonych, wariant 'In_numbers' dla wartosci ktore mogą należeć do danego przedziału, a 
    wariant 'Out_numbers' dla wartości które mogą neleżeć do dwóch przedziałów z których pierwszy nie ma ograniczenia dolnego a drugi górnego,
    wtedy 'Out_numbers' zawiera ograniczenie górne pierwszego z tych przedziałów i ograniczenie dolne drugiego z nich *)
    type wartosc =
      | Non
      | In_numbers of float * float 
      | Out_numbers of float * float

    let wartosc_dokladnosc x p =
      let x1 = x *. (100.0 +. p) /. 100.0 in
      let x2 = x *. (100.0 -. p) /. 100.0 in
      In_numbers (min x1 x2, max x1 x2);;

    let wartosc_od_do x y =
      In_numbers (x, y);;
    
    let wartosc_dokladna x =
      In_numbers (x, x);;
    
    let in_wartosc x y =
      match x with
        | Non -> false 
        | Out_numbers (o1, o2) -> 
          if y <= o1 || y >= o2 then true
          else false
        | In_numbers (i1, i2) ->
          if y >= i1 && y <= i2 then true
          else false;;

    (* funkcja 'przeciwieństwo' dla zbioru A zwraca zbiór -A. Bierze ona jako argument i zwraca wariant typu 'wartosc' i
    zachodzi: przeciwieństwo x = minus (wartosc_dokladna 0.0) x *)
    let przeciwienstwo x =
      match x with
        | Non -> Non
        | Out_numbers (o1, o2) -> Out_numbers (-.o2, -.o1)
        | In_numbers (i1, i2) -> In_numbers (-.i2, -.i1);;
    
    let min_wartosc x =
      match x with
        | In_numbers (a, _) -> a
        | Non -> nan
        | _ -> neg_infinity;;

    let max_wartosc x =
      match x with
        | Non -> nan
        | In_numbers (_, a) -> a 
        | _ -> infinity;;

    let sr_wartosc x =
      match x with
        | In_numbers (a, b) -> ((a +. b) /. 2.0)
        | _ -> nan;;

    (* funkcja 'dodawanie_in_out' służy do dodania typów 'wartość'- pierwszego w wariancie 'In_numbers' drugiego w wariancie 'Out_numbers'.
    Funkcja ta jako argument przyjmuje dwie pary floatow opisujace oba zbiory wartosci, a jako wynik daje odpowiedni wariant typu 'wartosc' *)    
    let dodawanie_in_out (x1, x2) (y1, y2) =
      let z1 = y1 +. x2 in
      let z2 = y2 +. x1 in
      if z1 < z2 then Out_numbers (z1, z2)
      else In_numbers (neg_infinity, infinity);;
      
    let plus x y =
      match (x, y) with
        | (Non, _) -> Non
        | (_, Non) -> Non
        | (Out_numbers (_, _), Out_numbers (_, _)) -> In_numbers (neg_infinity, infinity)
        | (In_numbers (ia1, ia2), In_numbers (ib1, ib2)) -> In_numbers (ia1 +. ib1, ia2 +. ib2)
        | (In_numbers (i1, i2), Out_numbers (o1, o2)) -> dodawanie_in_out (i1, i2) (o1, o2)
        | (Out_numbers (o3, o4), In_numbers (i3, i4)) -> dodawanie_in_out (i3, i4) (o3, o4);;

    (* odejmowanie od zbioru A zbioru B to tak naprawdę dodawanie do zbioru A zbioru -B *) 
    let minus x y =
      plus x (przeciwienstwo y);;

    (* funkcja 'mnozenie_in_out' służy do mnożenia typów 'wartość'- pierwszego w wariancie 'In_numbers' drugiego w wariancie 'Out_numbers'. 
    Funkcja ta jako argument przyjmuje dwie pary floatow opisujace oba zbiory wartosci, a jako wynik daje odpowieddni wariant typu 'wartosc' *)
    let mnozenie_in_out (x1, x2) (y1, y2) =
      if x1 = 0.0 && x2 = 0.0 then
        In_numbers (0.0, 0.0)
      else if x1 <= 0.0 && x2 >= 0.0 then
        In_numbers (neg_infinity, infinity)
      else if x1 < 0.0 then
        przeciwienstwo (Out_numbers (max (-.x1 *. y1) (-.x2 *. y1), min (-.x1 *. y2) (-.x2 *. y2)))
      else
        Out_numbers (max (x1 *. y1) (x2 *. y1), min (x1 *. y2) (x2 *. y2));;
         
     (* funkcja xy rozwiązuje problem polegający na tym że dla OCamla infinity*0.0 := nan a na potrzeby zadania potrzebowałem równości infinity*0.0 := 0.0 *)   
    let xy x y =
      if x = 0.0 || y = 0.0 then 0.0
      else x *. y;;

    let razy x y =
      match (x, y) with
        | (Non, _) -> Non
        | (_, Non) -> Non
        | (In_numbers (ia1, ia2), In_numbers (ib1, ib2)) -> In_numbers ((min (min (xy ia1 ib1) (xy ia1 ib2)) (min (xy ia2 ib1) (xy ia2 ib2))),
                                                                        (max (max (xy ia1 ib1) (xy ia1 ib2)) (max (xy ia2 ib1) (xy ia2 ib2))))
        | (In_numbers (i1, i2), Out_numbers (o1, o2)) -> mnozenie_in_out (i1, i2) (o1, o2)
        | (Out_numbers (o3, o4), In_numbers (i3, i4)) -> mnozenie_in_out (i3, i4) (o3, o4)
        | (Out_numbers (oa1, oa2), Out_numbers (ob1, ob2)) -> Out_numbers (max (oa1 *. ob2) (oa2 *. ob1), min (oa1 *. ob1) (oa2 *. ob2));;

    (* funkcja 'odwrotnosc' jest przyjmuje typ 'wartosc' i dla danego w nim zbioru wartosci A/{0} zwraca 
    zbiór wartosci (A/{0))^(-1) będących odwrotnościami wartości z A, innymi słowy zachodzi: odwrotność x = podzielic (wartosc_dokladna 1.0) x *)
    let odwrotnosc x =
      match x with
        | Non -> Non
        | In_numbers (i1, i2) ->
          if i1 = 0.0 && i2 = 0.0 then
            Non
          else if i1 < 0.0 && i2 > 0.0 then
            Out_numbers (1.0 /. i1, 1.0 /. i2)
          else if i1 < 0.0 then
            przeciwienstwo (In_numbers (1.0 /. (abs_float i1), 1.0 /. (abs_float i2)))
          else 
            In_numbers (1.0 /. i2, 1.0 /. i1)
        | Out_numbers (o1, o2) -> 
          if o1 > 0.0 || o2 < 0.0 then
            Out_numbers (1.0 /. o2, 1.0 /. o1)
          else if o2 = 0.0 then
            In_numbers (neg_infinity, 1.0 /. o1)
          else if o1 = 0.0 then
            In_numbers (1.0 /. o2, infinity)
          else 
            In_numbers (1.0 /. o1, 1.0 /. o2);;

    (* korzystam tu z faktu że dzielenie to tak naprawdę mnożenie przez odwrotnosc *) 
    let podzielic x y =
      razy x (odwrotnosc y);;


      (* Trochę testów *)
(*  let is_nan x = compare x nan = 0;;
   
    let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
    let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
    let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
    let d = plus c a                          (* (-inf, inf) *)
    let e = wartosc_dokladna 0.               (* <0, 0> *)
    let f = razy c e                          (* <0, 0> *)
    let g = razy d e                          (* <0, 0> *)
    let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
    let i = podzielic h e                     (* nan, przedzial pusty*)
    let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
    let k = razy j j                          (* <-30, 36> *)
    let l = plus a b                          (* <-2, 0> *)
    let m = razy b l                          (* <0, 2> *)
    let n = podzielic l l                     (* <0, inf) *)
    let o = podzielic l m                     (* (-inf, 0) *)
    let p = razy o a                          (* (-inf, inf) *)
    let q = plus n o                          (* (-inf, inf) *)
    let r = minus n n                         (* (-inf, inf) *)
    let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
    let t = razy n s;;                        (* (-inf, 0) *)
    
    assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
    assert (is_nan (sr_wartosc c) );
    assert (not (in_wartosc c 0.));
    assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
    assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
    assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
    assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
    assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
    assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
    assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
    assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
    assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
    assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
    assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
    assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
    assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;
    *)