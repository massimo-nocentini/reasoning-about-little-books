

signature SEXP_EQUAL = 
	sig
		type 'a sexp
		val equal: ('a -> 'b -> bool) -> 'a sexp -> 'b sexp -> bool
	end

functor SexpEqual (
	structure Sexp: SEXP) 
	:> SEXP_EQUAL where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun equal eq_fn fst_sexp snd_sexp = 
	    let
			fun equal_sexp (Atom a) (Atom b) = eq_fn a b
			|	equal_sexp (List fst_list) (List snd_list) = 
					equal_slist fst_list snd_list
			|	equal_sexp _ _ = false
			and equal_slist Null Null = true
			|	equal_slist (Cons (fst_cons_sexp, fst_cons_slist))
							(Cons (snd_cons_sexp, snd_cons_slist)) = 
					equal_sexp fst_cons_sexp snd_cons_sexp andalso
					equal_slist fst_cons_slist snd_cons_slist
			|	equal_slist _ _ = false
	    in equal_sexp fst_sexp snd_sexp end

	end
