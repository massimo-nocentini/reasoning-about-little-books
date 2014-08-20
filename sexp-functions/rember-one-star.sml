


signature SEXP_REMBER_ONE_STAR =
	sig
		type 'a sexp
		
		val rember_one_star : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end


functor SexpRemberOneStar (
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp )
	:> SEXP_REMBER_ONE_STAR where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun rember_one_star (List slist) (Atom target) comparer = 
		let
			fun R (empty as Null) = empty
			|	R (Cons ((atom_sexp as Atom atom), cdr_slist)) = 
					if comparer atom target
					then cdr_slist else Cons (atom_sexp, R cdr_slist)
			|	R (Cons ((list_sexp as List slist), cdr_slist)) =
					let val processed = List (R slist) in	
						if SexpEqualFunction.equal comparer list_sexp processed
						then Cons (list_sexp, R cdr_slist) 
						else Cons (processed, cdr_slist) 
					end


		in List (R slist)  end


	end





