
signature SEXP_SUBST = 
sig
    type 'a sexp

    val subst : 'a sexp -> ('a -> bool) -> 'a -> 'a sexp
end

functor SexpSubst (structure Sexp : SEXP)
	:> SEXP_SUBST where type 'a sexp = 'a Sexp.sexp
        =
	struct

	open Sexp
		 
	fun subst sexp pred new =
	    let

		fun subst_in_slist Null = Null
		  | subst_in_slist (Cons (sexp, slist)) = 
		    Cons ((subst_in_sexp sexp), (subst_in_slist slist))
		and subst_in_sexp (original as (Atom atom)) =
		    if pred atom then Atom new else original
		  | subst_in_sexp (List aList) =
		    List (subst_in_slist aList) 

	    in subst_in_sexp sexp end

	end
