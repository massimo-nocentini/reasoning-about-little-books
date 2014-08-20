

signature SEXP_DEPTH_STAR =
	sig
		type 'a sexp

		val depth_star : 'a sexp -> int
	end

functor SexpDepthStar (
	structure Sexp : SEXP)
	:> SEXP_DEPTH_STAR where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun depth_star (Atom _) = 0
	|	depth_star (List slist) = 
		let
			fun D Null = 1
			|	D (Cons (Atom _, cdr_slist)) = D cdr_slist
			|	D (Cons (List car_slist, cdr_slist)) = 
					if 1 + (D car_slist) < D cdr_slist
					then D cdr_slist
					else 1 + (D car_slist)

			fun D' Null = 1
			|	D' (Cons (Atom _, cdr_slist)) = D' cdr_slist
			|	D' (Cons (List car_slist, cdr_slist)) = 
				let
					val a = 1 + (D' car_slist)
					val d = D' cdr_slist
				in Int.max (a,d) end
		in D' slist end

	end
