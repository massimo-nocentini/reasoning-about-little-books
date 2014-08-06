
signature SEXP_PICK = 
	sig
		type 'a sexp

		exception IndexOutOfBound

		val pick : 'a sexp -> int -> 'a sexp

	end

functor SexpPick (structure Sexp : SEXP)
	:> SEXP_PICK where type 'a sexp = 'a Sexp.sexp
	=
	struct

		exception IndexOutOfBound

		open Sexp

		fun pick (List conses) n = 
			let 
				fun	pick_slist _ Null = raise IndexOutOfBound 
				|	pick_slist 1 (Cons (sexp, _)) = sexp
				|	pick_slist n (Cons (_, cdr_slist)) = pick_slist (n-1) cdr_slist
			in pick_slist n conses end
	end


