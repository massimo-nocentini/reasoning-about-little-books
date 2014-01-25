functor SexpFunctionsStandardImpl (structure Sexp: SEXP)
	:> SEXP_FUNCTIONS where type 'a sexp = 'a Sexp.sexp
	=
	struct

	(* The functor type checks correctly even if the following two
	 type definitions are omitted.*)
	type 'a sexp = 'a Sexp.sexp
	type 'a slist = 'a Sexp.slist

	open Sexp

	fun to_string toStringFun aSexp =
	    let 
		fun sexp_to_string (Atom anAtom) = toStringFun anAtom
		  | sexp_to_string (List aList) = "(" ^ (slist_to_string aList) ^ ")"
		and slist_to_string Null = ""
		  | slist_to_string (Cons (aSexp, Null)) = sexp_to_string aSexp
		  | slist_to_string (Cons (aSexp, aList)) = 
		    (sexp_to_string aSexp) ^ " " ^ (slist_to_string aList)
	    in
		sexp_to_string aSexp
	    end

	fun is_atom (Atom _) = true
	  | is_atom _ = false

	fun equal eq_fn fst_sexp snd_sexp = 
	    let
		fun equal_sexp (Atom a) (Atom b) = eq_fn a b
		  | equal_sexp (List fst_list) (List snd_list) = 
		    equal_slist fst_list snd_list
		  | equal_sexp _ _ = false
		and equal_slist Null Null = true
		  | equal_slist (Cons (fst_cons_sexp, fst_cons_slist))
				(Cons (snd_cons_sexp, snd_cons_slist)) = 
		    (equal_sexp fst_cons_sexp snd_cons_sexp) andalso
		    (equal_slist fst_cons_slist snd_cons_slist)
		  | equal_slist _ _ = false
	    in
		equal_sexp fst_sexp snd_sexp
	    end

	end
