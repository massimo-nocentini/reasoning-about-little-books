
signature SEXP_MEMBER = 
	sig
		type 'a sexp

		val member : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> bool
	end

functor SexpMember (
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp )
	:> SEXP_MEMBER where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
		open Sexp

		fun member (List slist) target_sexp equality_comparer =
			let 
				fun M Null = false
				|	M (Cons (car, cdr)) = 
					if SexpEqualFunction.equal equality_comparer car target_sexp
					then true else M cdr
			in M slist end

	end
