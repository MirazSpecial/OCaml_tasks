(**************************************)
(* Zadanie:               Przelewanka *)
(* Autor:            Konrad Litwinski *)
(* Recenzent:        Maciek Sygnowski *)
(**************************************)


let is_possible lst0 =
  (* 'is_possible' sprawza czy opisane rozlanie jest mozliwe do otrzymania *)
  let rec gcd num1 num2 =
    (* 'gcd' zwraca NWD z podanych liczb przy zalozeniu, ze num1 >= num2 *)
    if num2 = 1 || num1 mod num2 = 0 then
      num2
    else
      gcd num2 (num1 mod num2)
  in
  let (default_gcd, _) = List.hd lst0 in
  let gcd_of_all = List.fold_left (fun a (x, _) -> gcd (max a x) (min a x)) default_gcd lst0 in
  let is_devided = List.for_all (fun (_, y) -> y mod gcd_of_all = 0) lst0 in
  let is_empty_full = List.exists(fun (x, y) -> if y = 0 || x = y then true else false) lst0 in
  (* by sytuacja byla mozliwa do otrzymania finalna ilosc wody w kazdym naczyniu musi byc podzielna przez 
  NWD z wielkosci wszystkich naczyn oraz musi istniec przynajmniej jedno niezerowe naczynie puste lub pelne *)
  (is_devided && is_empty_full);;


let simulate lst0 =
  let n = List.length lst0 in
  (* 'h_table' to hashtable sluzaca do sprawdzania czy dany uklad napelnienia zostal wczesniej osiagniety *)
  let h_table = Hashtbl.create (n) in
  (* 'que' to kolejka kolejnych obrabianych ukladow napelnienia potrzebna do BFSa *)
  let que = Queue.create () in
  let (state0, final_state) = List.split lst0 in
  (* 'state0' to tablica pojemnosci naczyn *)
  let state0 = Array.of_list state0 in
  (* 'final_state' to tablica docelowych napelnien *)
  let final_state = Array.of_list final_state in
    
  let pour arr spot1 spot2 =
    (* 'pour' dla danej tablicy 'arr' "przelewa" wartosc z pola 'spot1' na 'spot2' *)
    let left_space = state0.(spot2) - arr.(spot2) in
    let pour_quantity = min arr.(spot1) left_space in
    arr.(spot1) <- (arr.(spot1) - pour_quantity);
    arr.(spot2) <- (arr.(spot2) + pour_quantity);
  in

  let maybe_add (state, steps) =
    (* 'maybe_add' sprawdza czy dany uklad napelnienia zostal juz uzyskany i
    dodaje go do kolejki w przeciwnym razie *)
    if not (Hashtbl.mem h_table state) then (
      Hashtbl.add h_table state 1;
      Queue.add (state, steps) que; )
  in
  
  let generate_states state (steps:int) =
  (* 'generate_states' dla pewnego ukladu napelnienia naczyn wykonuje 'maybe_add' 
  dla kazdego ukladu mozliwego do otrzymania przez wykonanie jednego przelania *)  
    for i = 0 to n - 1 do
      (* wylania wody z i-tego naczynia *)
      if (state.(i) <> 0) then (
      let new_state = Array.copy state in
      new_state.(i) <- 0;
      maybe_add (new_state, steps);
      );
    done;
    for i = 0 to n - 1 do
      (* dolanie z kranu do i-tego naczynia *)
      let new_state = Array.copy state in
      new_state.(i) <- state0.(i);
      maybe_add (new_state, steps);  
    done;
    for i = 0 to n - 1 do
      for j = 0 to i - 1 do
        (* przelewanie wody z i-tego do j-tego naczynia *)
        if (state.(i) <> 0) then (
        let new_state = Array.copy state in
          pour new_state i j;
          maybe_add (new_state, steps); 
        );
        (* przelewanie wody z j-tego do i-tego naczynia *)
        if (state.(j) <> 0) then (
        let new_state = Array.copy state in
          pour new_state j i;
          maybe_add (new_state, steps);
        );
      done;
    done;
  in
    
  let found = ref false in
  let result = ref 0 in
  maybe_add (Array.init n (fun _ -> 0), 0);
  (* ponizsza petla bedzie wykonywana az do momentu gdy otrzymamy uklad napelnienia
  odpowiadajacy 'final_state' *)
  while not !found do
    begin
      let (state, steps) = Queue.take que in
      (* print_int(steps); *)
      result := steps;
      if state = final_state then
        found := true
      else
        generate_states state (steps + 1);
    end
  done;
  !result;;


let przelewanka arr0 =
  (* 'przelewanka' tak jak w tresci *)
  let lst0 = Array.fold_left (fun a (x, y) -> if x <> 0 then (x, y) :: a else a) [] arr0 in
  if List.length lst0 = 0 then
    0
  else if not (is_possible lst0) then
    -1
  else 
    simulate lst0;;


    
(****************************** TESTY ******************************)
    
(*
  let a = [|(100, 50); (1000, 500); (50, 25); (5, 5)|];;
    assert(20 = przelewanka a);;
    
  let a = [|(1, 0); (2, 1); (4, 3); (8, 7); (16, 15); (32, 31)|];;
    assert(11 = przelewanka a);;
    
  let a = [|(100, 33); (25, 11); (13, 11); (3, 0); (1, 1)|];;
    assert(13 = przelewanka a);;
    
  let a = [|(1, 0); (2, 1); (3, 1); (4, 2); (5, 2); (6, 3); (7, 3); (8, 4)|];;
    assert(10 = przelewanka a);;
    
  let a = [|(6, 3); (9, 3); (12, 3); (15, 3); (18, 3); (477, 3); (0, 0)|];;
    assert(-1 = przelewanka a);;
    
  let a = Array.init 35 (fun i -> (i * i, if i = 25 then 20 * 20 else 0));;
    assert(2 = przelewanka a);;
*)