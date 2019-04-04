(* Sum type to encode efficiently polynomial expressions *)
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
let rec from_expr (_e: Expr.expr) : pExp = match _e with 
  | Num(i) -> Term(i,0);
  | Var(c) -> Term(1,1);
  | Add(e1,e2) -> 
    let _e1 = from_expr e1 in let _e2 = from_expr e2 in Plus([_e1; _e2]);
  (* | Sub(e1,e2) -> Plus([from_expr e1; from_expr Neg(e2)]) 
  | Mul(e1,e2) -> let _e1 = from_expr e1 in let _e2 = from_expr e2 in match _e1, _e2 with
    | (Term(c1,v1), Term(c2,v2)) -> Term(c1*c2,v1+v2)
    | _ -> Times([_e1; _e2])
  | Pow(e,i) -> let _ex = from_expr e in match _ex with 
    | Term(c,p) -> Term(c ** i, p * i)
    | Plus(plist) -> Term (0,0) (* TODO *)*)
  (* | Pos(e) -> from_expr e;
  | Neg(e) -> let _n = from_expr e in match _n with 
    | Term(c,v) -> Term(-1*c,v)
    | _ -> Times([Term(-1,0);_n]) *)
  | _ -> Term(0,0) (* TODO *);;

(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)
let degree (_e:pExp): int = 0 (* TODO *)

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare (e1: pExp) (e2: pExp) : bool =
  degree e1 > degree e2

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
    match plist with 
      | [] -> ();
      | [_t] -> do_print _t; 
      | _t::tl ->  match _t with 
        | pExp -> 
          Printf.printf "(";
          do_print _t;
          Printf.printf " + ";
          do_print (Plus(tl));
          Printf.printf ")";
        | _ -> ();
  | _ -> Printf.printf "Not implemented\n" ;;

let print_pExp (_e: pExp): unit = 
  do_print _e;
  Printf.printf "\n";;




  (* TODO *)
  

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
let simplify1 (e:pExp): pExp =
    e;;

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  true;;

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      print_pExp rE;
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)
;;



