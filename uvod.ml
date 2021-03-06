(* ===== Vaja 1: Uvod v OCaml  ===== *)


(* Funkcija "predzadnji_element l" vrne predzadnji element seznama l. 
 V primeru prekratkega seznama vrne napako.
 ----------
 # predzadnji_element [1; 2; 3; 4];;
 - : int = 3
 ---------- *)

let rec predzadnji_element l = 
	match l with
	| [] -> failwith "Prekratek seznam."
	| [x] -> failwith "Prekratek seznam."
	| [x;y] -> x
	| hd::tl -> predzadnji_element tl
	
let rec predzadnji_element2 l = function 
	| [] | [_] -> failwith "Prekratek seznam."
	| [x;_] -> x
	| _::tl-> predzadnji_element2 tl

(* Funkcija "poisci k l" poišče k-ti element v seznamu l.
 Številčenje elementov seznama (kot ponavadi) pričnemo z 0.
 Privzamemo, da je k nenegativno število.
 V primeru prekratkega seznama funkcija vrne napako.
 ----------
 # poisci 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
 ---------- *)

let rec poisci k l = 
	match (k,l) with
	| (_,[]) -> failwith "Prekratek seznam."
	| (0, hd::tl) -> hd
	| (_,hd::tl) -> poisci (k-1) tl


(* Funkcija "podvoji l" podvoji pojavitve elementov v seznamu l.
 ----------
 # podvoji [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
 ---------- *)

let rec podvoji l = 
	match l with
	| [] -> []
	| [x] -> [x] @ [x]
	| hd::tl -> hd :: hd :: podvoji tl

(* Funkcija "razdeli k l" seznam l razdeli na dva seznama. Prvi vsebuje prvih k elementov
 seznama l, v drugem pa vsi ostali. Funkcija vrne par teh dveh seznamov.
 V primeru, ko je k izven mej seznama, je bo primeren od seznamov enak [])
 ----------
 # razdeli 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # razdeli 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
 ---------- *)
 
let rec razdeli k l = 
	match (k,l) with
	| (0,l) -> ([],l)
	| (k,[]) -> ([],[])
	| (k,hd::tl) ->
		let (l1,l2) = razdeli (k-1) tl in
		(hd::l1,l2)

let rec razdeli2 k l =
	let rec prenasalec (l1,l2) k =
		match (k,l2) with
		| (_,[]) -> (l1,l2)
		| (0,_) -> (l1,l2)
		| (k,hd::tl) -> prenasalec (l1@[hd],tl) (k-1)
	in 
	prenasalec ([],l) k
	
	
(* Funkcija "zbrisi k l" iz seznama l pobriše k-ti element.
 V primeru prekratkega seznama funkcija vrne napako.
 ----------
 # zbrisi 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
 ---------- *)
 
let rec zbrisi k l = 
	match (k,l) with
	| (_,[]) -> failwith "Prekratek seznam."
	| (k,l) -> let l1,l2 = razdeli k l in
		match l2 with
		| [] -> failwith "Prekratek seznam."
		| hd::tl -> l1 @ tl
	

(* Funkcija "rezina i k l" sestavi novi seznam, ki vsebuje elemente seznama l od vključno
 i-tega do k-tega (brez k-tega).
 Predpostavimo, da sta i in k primerna.
 ----------
 # rezina 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
 ---------- *)
 
let rec rezina i k l = 
	let (_, rez1) = razdeli i l in 
	let (rez2, _) = razdeli (k-i) rez1 in rez2

(* Funkcija "vstavi x k l" na k-to mesto seznama l vrine element x.
 Če je k izven mej seznama, ga doda na začetek oz. konec.
 ----------
 # vstavi 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # vstavi 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
 ---------- *)

let vstavi x k l = 
	let (zacetek, konec) = razdeli l k in
	zacetek @ [x] @ konec
	

(* Funkcija "zavrti n l" seznam l zavrti za n mest v levo.
 Predpostavimo, da je n v mejah seznama.
 ----------
 # zavrti 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
 ---------- *)

let rec zavrti n l = 
	let (zacetek, konec) = razdeli n l in
	konec @ zacetek
 
(* Funkcija "pobrisi x l" iz seznam l izbriše vse pojavitve elementa x.
 ----------
 # pobrisi 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
 ---------- *)

let rec pobrisi x l = 
	match x l with
	| [] -> []
	| hd :: tl -> if hd = x then pobrisi x tl else hd :: pobrisi x tl


(* Funkcija "je_palindrom l" ugotovi ali seznam l predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama. 
 ----------
 # je_palindrom [1; 2; 3; 2; 1];;
 - : bool = true
 # je_palindrom [0; 0; 1; 0];;
 - : bool = false
 ---------- *)
let rec reverse l = function
	| [] -> []
	| hd :: tl -> reverse tl @ [hd]
 
let je_palindrom l = function
	if l= reverse l then true else false
	
let je_palindrom' l =
	let rec obrni = function
		| [] -> []
		| hd :: tl -> obrni tl @ [hd]
	in
	l = obrni l
  
(* Funkcija "max_po_komponentah l1 l2" vrne seznam, ki ima za elemente
 večjega od elementov na ustreznih mestih v seznamih l1 in l2.
 Skupni seznam ima dolžino krajšega od seznamov l1 in l2. 
 ----------
 # max_po_komponentah [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
 ---------- *)
let max_po_komponentah l1 l2 = 
	match (l1, l2) with
	| (_, []) | ([], _) -> []
	| (x::xs, y::ys) ->  max x y :: max_po_komponentah xs ys
  
(* Funkcija "drugi_najvecji l" vrne drugo največjo vrednost v seznamu l.
 Ponovitve elementa se štejejo kot ena vrednost.
 Predpostavimo, da ima seznam vsaj dva različna elementa.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu. 
 ----------
 # drugi_najvecji [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
 ---------- *)
 
let drugi_najvecji l = 
	let rec najvecji l = function
		| [] -> failwith "Prekratek seznam"
		| [x] -> xs
		| hd :: tl -> max hd (najvecji tl)
	in
	najvecji(pobrisi(najvecji l) l)
	