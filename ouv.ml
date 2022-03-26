type monome = { c : int; d : int; };; (** Definition du type monome *)
type polynome = monome list;; (** Definition linéaire du type polynome *)



let monComp a b = a.d - b.d;; (** Tri croissant par rapport au degré du monôme *)
let pol = List.sort monComp [{c=2;d=3}; {c=9;d=4}; {c=2;d=3}; {c=9;d=5}; {c=9;d=1}];; 
let rec remDup pol = match pol with (** Fonction de suppression des duplications des monômes du même degré *)
          [] -> []
          |[{c;d}] -> [{c;d}]
          |a::b::rest -> if a.d = b.d then {c=a.c+b.c; d=a.d}::remDup rest else a::remDup (b::remDup rest);;
let rec remZero pol = match pol with (** Suppression des monômes dont le coéfficient = 0 *)
            [] -> []
            |a::rest -> if a.c = 0 then remZero rest else a::remZero rest;;

let canonique pol = let (pol:polynome) = List.sort monComp pol in let pol = remDup pol in remZero pol;; (** La fonction 'canonique' *)

let  poly_add pol1 pol2 = 
let rec poly_add_aux pol1 pol2 = 
match pol1, pol2 with (** Addition de deux polynomes canoniques *)
            [],[] -> []
            |pol1, [] -> pol1
            |[], pol2 -> pol2
            |a1::rest1, a2::rest2 -> if a1.d = a2.d then ( {c=a1.c + a2.c; d=a1.d}::poly_add_aux rest1 rest2 )else if a1.d < a2.d then a1::(poly_add_aux rest1 (a2::rest2)) else a2::(poly_add_aux (a1::rest1) rest2)
in remZero(poly_add_aux pol1 pol2);;

let rec poly_prod pol1 pol2 = match pol1, pol2 with (** Produit de deux polynomes cannoniques *)
            [],[] -> []
            |pol1, [] -> []
            |[],pol2 -> []
            |a1::rest1, a2::rest2 ->poly_add (poly_add [{c=a1.c*a2.c;d=a1.d+a2.d}] (poly_prod [a1] rest2)) (poly_prod rest1 (a2::rest2));;


type e =   (** Structure arborescente d'une expression *)
    Int of int
  | Chap of  int
  | Plus of  plusChild list
  | Prod of  prodChild list
and plusChild =
    Int2 of int
  | Chap2 of int
  | Prod2 of  prodChild list
and prodChild =
    Int3 of int
  | Chap3 of int
  | Plus2 of plusChild list;;

  let arbreEq = Plus([Prod2([Int3(123);Chap3(1)]);Int2(42);Chap2(3)]);;
 
    let rec arb2poly_plus a = match a with
            | []->[]
            |Int2 (x:int)::rest -> poly_add ([{c=x;d=0}]) (arb2poly_plus rest)
            |Chap2(x:int)::rest -> poly_add ([{c=1;d=x}]) (arb2poly_plus rest)
            |(Prod2(b::rest))::rest2 ->poly_add (poly_prod (arb2poly_prod [b]) (arb2poly_prod rest)) (arb2poly_plus rest2) and
     arb2poly_prod a = match a with
            | [] -> [{c=1;d=0}]
            |Int3 (x:int)::rest -> poly_prod ([{c=x;d=0}]) (arb2poly_prod rest)
            |Chap3(x:int)::rest -> poly_prod ([{c=1;d=x}]) (arb2poly_prod rest)
            |Plus2(b::rest)::rest2 ->poly_prod (poly_add (arb2poly_plus [b]) (arb2poly_plus rest))(arb2poly_prod rest2);;

    let rec arb2poly a = match a with (** Transformer un arbre en polynome *)
            | Plus([]) -> []
            |Prod([]) -> []
            |Int (x:int) -> [{c=x;d=0}]
            |Chap(x:int) -> [{c=1;d=x}]
            |Plus(b::rest) -> poly_add (arb2poly_plus [b]) (arb2poly_plus rest)
            |Prod(b::rest) -> poly_prod (arb2poly_prod [b]) (arb2poly_prod rest);;



    let extraction_alea (a:int list) (b:int list) = match a,b with (** Extraction aléatoire *)
        | [],b -> ([],b)
        |a,b -> (
            let l = List.length a in let r = (Random.int (l) + 1 ) in (
                let rec extract_aux lst i= match lst with
                [] -> []
                |a::rest -> if (i=1) then rest else a::(extract_aux rest (i-1))
                in ((extract_aux a r),((List.nth a (r-1)::b))
            ) 
        ) )

    let gen_permutation n = ( (** Génération d'une liste d'entiers d'ordre aléatoire *)
        
        let rec unfold_right f init =( (** fonction pour aider à générer une liste croissante d'entiers *)
    match f init with
    | None -> []
    | Some (x, next) -> x :: unfold_right f next)
     in let range n =
    (let irange x = if x > n then None else Some (x, x + 1) in
    unfold_right irange 1)
    in let l = (range n) in (
        let rec gen (x,y)= 
            
            match (x,y) with 
            |([],p) -> ([],p)
            |(x,y) -> gen (extraction_alea x y)
        in (gen (range (n),[]))
    ));;
let getList c = match c with (_,a) -> a;;


    type arbre = Feuille
                    | Noeud of int*arbre*arbre
                    | Noeud2 of string*arbre*arbre;;
                    

let abr l =
let l = List.rev l in 
    let rec abr_aux l =
        
        let rec inserer v = function
            | Feuille -> Noeud (v, Feuille, Feuille)
            | Noeud (r, fg ,fd) ->
                if v < r then Noeud ( r,inserer v fg, fd)
                else Noeud (r, fg,  inserer v fd)
        in match l with 
            [] -> Feuille
            | t::q -> inserer t (abr_aux q) in abr_aux l
            ;;

 let etiquetage a = 
    let rec etiquetage_aux = function 
    | Noeud(v,Feuille,Feuille) -> (if (v mod 2 = 1) then (let vr = ((Random.int 401) - 200) in Noeud2("*",Noeud(vr,Feuille,Feuille),Noeud2("x",Feuille,Feuille)))
                                                    else (Noeud2("^",Noeud2("x",Feuille,Feuille),Noeud((Random.int 101), Feuille,Feuille))))
    | Noeud(l, fg,fd) -> let p = Random.int 100 in if (p<75) then Noeud2("+",etiquetage_aux fg, etiquetage_aux fd) else Noeud2("*",etiquetage_aux fg, etiquetage_aux fd)
    |Feuille -> let p = Random.int 100 in (if (p<50) then Noeud(((Random.int 401) - 200),Feuille,Feuille) else Noeud2("x",Feuille,Feuille)) in etiquetage_aux a;;

let rec merge list1 list2 = 
    match list1, list2 with
        | [], _ -> list2
        | hd :: tl, _ -> hd :: merge tl list2 ;;
let rec gen_arb a = match a with
    | Noeud(v,_,_) -> Int(v)
    | Noeud2("+",g,d) -> Plus(merge (traite g) (traite d))
    | Noeud2("*",g,d)->Prod(merge (traite2 g) (traite2 d))
    | Noeud2("^",g,Noeud(v,_,_)) -> Chap(v)
    | Noeud2("x",_,_) -> Chap(1)

    and  gen_arb_plus a = match a with
    | Noeud(v,_,_) -> Int2(v)
    | Noeud2("^",_,Noeud(v,_,_)) -> Chap2(v)
    | Noeud2("x",_,_) -> Chap2(1)
    | Noeud2("*",g,d) -> Prod2(merge (traite2 g) (traite2 d))
    and  gen_arb_prod a = match a with
    | Noeud(v,_,_) -> Int3(v)
    | Noeud2("^",_,Noeud(v,_,_)) -> Chap3(v)
    | Noeud2("x",_,_) -> Chap3(1)
    | Noeud2("+",g,d) -> Plus2(merge (traite g) (traite d))

    and traite a = match a with 
    | Noeud2("+",g,d) -> merge (traite g) (traite d)
    | a -> (gen_arb_plus a)::[]
    and traite2 a = match a with
    | Noeud2("*",g,d) -> merge (traite2 g) (traite2 d)
    | a -> (gen_arb_prod a)::[]
;;


    let repArbre num =
     let rec helper = fun n acc ->
          if n > 0
          then helper (n-1) ((gen_arb (etiquetage (abr (getList(gen_permutation 20))))) :: acc)
          else acc
     in
          helper num [];;

let time f x =
    let start = Unix.gettimeofday ()
    in let res = f x
    in let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
    in
       res;;

let rec sumArbre liste = match liste with 
    | [] -> []
    | [a] -> arb2poly a
    | a::rest -> poly_add (arb2poly a) (sumArbre rest)    
    ;;
let rec prodArbre liste = match liste with 
    | [] -> [{c=1;d=0}]
    | [a] -> arb2poly a
    | a::rest -> poly_prod (arb2poly a) (prodArbre rest)    
    ;;
    time (sumArbre) (repArbre 100);;
    time (prodArbre) (repArbre 100);;
    let twoList = [1;1;2;4;8;16;32;64;128;256;512;1024;2048;4096;8192];;
    let abr152 = List.map gen_arb (List.map etiquetage (List.map abr (List.map getList (List.map gen_permutation twoList))));;

    (** temps d'additions : 150.678879s
        temps de multiplications : 2470.913477s *)
        sumArbre abr152;;
        prodArbre abr152;;