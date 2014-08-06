
signature SEXP_TO_STRING =
	sig

		type 'a sexp
		val to_string: ('a -> string) -> 'a sexp -> string

	end

functor SexpToString (structure Sexp: SEXP)
	:> SEXP_TO_STRING where type 'a sexp = 'a Sexp.sexp
	=
	struct

	(* The functor type checks correctly even if the following two
	 type definitions are omitted, since type 'a sexp is present 
	 in the very structure Sexp.*)
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

	end
