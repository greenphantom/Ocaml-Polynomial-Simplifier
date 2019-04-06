(* Sum type to encode efficiently polynomial expressions *)
open Expr

type pExp =
  | Term of int*int (*
      First int is the constant
      Second int is the power of x 
      10  -> Term(10,0)
      2x -> Term(2,1)
      3x^20 -> Term(3, 20)
    *)
  | Plus of pExp list
  (*
    List of terms added
    Plus([Term(2,1); Term(1,0)])
  *)
  | Times of pExp list (* List of terms multiplied *)


(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
;;

let rec from_pow (_e: pExp) (_p: pExp list) (_i: int) : pExp = 
    (if(_i > 0) then 
      (from_pow _e ([_e]@_p) (_i - 1))
    else
      Times(_p)) ;;

let rec from_expr (_e: expr) : pExp = match _e with 
  | Num(i) -> Term(i,0);
  | Var(c) -> Term(1,1);
  | Add(e1,e2) -> 
    let _e1 = from_expr e1 in let _e2 = from_expr e2 in (match _e1, _e2 with
      | Term(c1, v1), Term(c2, v2) -> if (v1 = v2) then Term(c1 + c2, v2) else Plus([_e1; _e2])
      | Term(c1, v1), Plus(plist) -> Plus([_e1]@plist)
      | Plus(plist), Term(c2, v2) -> Plus(plist@[_e2])
      | Plus(plist1), Plus(plist2) -> Plus(plist1@plist2)
      | _ -> Plus([_e1; _e2]))
  | Sub(e1,e2) -> Plus([(from_expr e1); (from_expr (Neg(e2)))]) 
  | Mul(e1,e2) -> (let _e1 = from_expr e1 in let _e2 = from_expr e2 in match _e1, _e2 with
    | (Term(c1,v1), Term(c2,v2)) -> Term(c1*c2,v1+v2)
    | Term(c1, v1), Times(plist) -> Times([_e1]@plist)
    | Times(plist), Term(c2, v2) -> Times(plist@[_e2])
    | Times(plist1), Times(plist2) -> Times(plist1@plist2)
    | _ -> Times([_e1; _e2]))
  | Pow(e,i) -> (let _ex = from_expr e in match _ex with 
    | Term(c,p) -> Term((float_of_int c) ** (float_of_int i) |> int_of_float, p * i)
    | _ -> (from_pow _ex [] i)
    | _ -> Term (0, 0)) (* TODO *)
  | Pos(e) -> from_expr e; 
  | Neg(e) -> (let _n = from_expr e in match _n with 
    | Term(c,v) -> Term(-1*c,v)
    | _ -> Times([Term(-1,0);_n]))
  | _ -> Term(0,0) (* TODO *);;

(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)


let rec total_exponent (_v: int) (_p: pExp list) : int =
  match _p with
    | [] -> _v
    | t::tl -> (match t with
      | Term(c,v) -> total_exponent (_v + v) tl;
      | Plus(plist) -> total_exponent (total_exponent _v plist) tl;
      | Times(plist) -> total_exponent (total_exponent _v plist) tl;)

  and largest_exponent (_v: int) (_p: pExp list) : int = 
    match _p with
      | [] -> _v
      | t::tl -> (match t with
        | Term(c,v) -> if (v > _v) then largest_exponent v tl else largest_exponent _v tl;
        | Plus(plist) -> largest_exponent (largest_exponent _v plist) tl;
        | Times(plist) -> largest_exponent (total_exponent _v plist) tl;)


let degree (_e: pExp): int = 
  match _e with
    | Term(c,v) -> v
    | Plus(plist) -> total_exponent 0 plist
    | Times(plist) -> largest_exponent 0 plist;;

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)

let compare (e1: pExp) (e2: pExp) : int = 
  let d1 = degree e1 in
    let d2 = degree e2 in 
      if (d1 > d2) then -1 else if (d1 = d2) then 0 else 1;;

(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()

  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)
  
let rec do_print (_e: pExp): unit = match _e with 
  | Term(x1,x2) ->  if (x2 = 0) then Printf.printf "%i" x1 else Printf.printf "%ix^%i" x1 x2;
  | Plus(plist) ->
    (match plist with 
      | [] -> ();
      | [_t] -> do_print _t; 
      | _t::tl ->  match _t with 
        | pExp -> 
          Printf.printf "(";
          print_sum plist;
          Printf.printf ")";
        | _ -> ();)
  | Times(plist) -> 
    (match plist with
      | [] -> ();
      | [_t] -> do_print _t;
      | _t::tl -> match _t with
        | pExpr ->
          Printf.printf "(";
          print_times plist;
          Printf.printf ")";
        | _ -> ();)
  | _ -> Printf.printf "Not implemented\n" 

and print_sum (_sum: pExp list): unit = (match _sum with
  | [] -> ();
  | [_t] -> do_print _t; 
  | _t::tl ->  
    (match _t with 
        | pExp -> 
          do_print _t;
          Printf.printf " + ";
          print_sum tl;
        | _ -> (); )  
)

and print_times (_mul: pExp list): unit = (match _mul with
  | [] -> ();
  | [_t] -> do_print _t; 
  | _t::tl ->  
    (match _t with 
        | pExp -> 
          do_print _t;
          Printf.printf " * ";
          print_times tl;
        | _ -> ());  
);;

let print_pExp (_e: pExp): unit = 
  do_print _e;
  Printf.printf "\n";;




  (* TODO *)
  
let rec sort (_s: pExp): pExp = (match _s with
  | Term(c,v) -> _s;
  | Plus(plist) -> Plus(List.sort compare (List.map sort plist))
  | Times(plist) -> Times(List.sort compare (List.map sort plist)));;

(* Sum flattening operations *)

let get_plist (_s: pExp): pExp list = (match _s with 
  | Plus(alist) -> alist;
  | _ -> [_s]);;

let has_Plus (l: pExp list): bool = let x = (List.filter (function pExp -> match pExp with Plus(plist) -> false | _ -> true) l) in (match x with 
  | [] -> true;
  | _ -> false);;

let rec sep (l: pExp list) (run: pExp list): pExp list = (match l with 
  | [] -> run;
  | a::tl -> if (has_Plus [a]) then sep (get_plist a @ tl) run else sep tl (run @ [a]));;

let flatten_sum (_s: pExp): pExp = (match _s with
  | Plus(plist) -> Plus(sep plist []);
  | _ -> _s);;

(* Multiply flatten opertations *)

let get_mlist (_m: pExp): pExp list = (match _m with 
  | Times(alist) -> alist;
  | _ -> [_m]);;

let has_Times (l: pExp list): bool = let x = (List.filter (function pExp -> match pExp with Times(plist) -> false | _ -> true) l) in (match x with 
  | [] -> true;
  | _ -> false);;

let rec combine (l: pExp list) (run: pExp list): pExp list = (match l with 
  | [] -> run;
  | a::tl -> if (has_Times [a]) then combine (get_mlist a @ tl) run else combine tl (run @ [a]));;

let flatten_multiply (_m: pExp): pExp = (match _m with
  | Times(plist) -> Times(combine plist []);
  | _ -> _m);;

(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)

let rec simplify_add (e: pExp list) (run: pExp list) : pExp = 
  match e with 
    | t1::t2::tl -> (match t1, t2 with
      | Term(c1, v1), Term(c2, v2) -> if (v1 = v2)
                                      then let t = Term(c1 + c2, v1) in 
                                        simplify_add ([t]@tl) (run)
                                        else simplify_add ([t2]@tl) (run@[t1])
      | _ -> Plus(run))
    | [t] -> Plus(run@[t])
    | _ -> Plus(run)

let simplify1 (e:pExp): pExp =
  (match e with
    | Term(c,v) -> e
    | Plus(plist) -> (let _e = sort (flatten_sum e) in match _e with 
                                        | Plus(plist1) -> simplify_add plist1 [])
    | _ -> e)

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) : bool =
  (match _e1, _e2 with 
    | Term(c1, v1), Term(c2,v2) -> _e1 = _e2
    | Plus(plist1), Plus(plist2) -> Plus(List.sort compare plist1) = Plus(List.sort compare plist2)
    | Times(plist1), Times(plist2) -> Times(List.sort compare plist1) = Times(List.sort compare plist2)
    | _ -> false);;


(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1 e in
      print_pExp rE;
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)
;;



