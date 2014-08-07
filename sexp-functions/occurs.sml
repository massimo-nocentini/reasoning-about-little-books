
signature SEXP_OCCURS = 
sig
    type 'a sexp

    val occurs : 'a sexp -> ('a -> bool) -> int
end

functor SexpOccurs (structure Sexp : SEXP)
	:> SEXP_OCCURS where type 'a sexp = 'a Sexp.sexp   
        =
	struct

	open Sexp

	fun occurs sexp pred = 
	    let 
		fun occurs_in_slist Null = 0
		  | occurs_in_slist (Cons (sexp, aList)) = 
		    (occurs_in_sexp sexp) + (occurs_in_slist aList)
		and occurs_in_sexp (Atom y) = 
		    if pred y then 1 else 0
		  | occurs_in_sexp (List aList) =
		    occurs_in_slist aList
	    in occurs_in_sexp sexp end

	end
