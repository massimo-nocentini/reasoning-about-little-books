
signature SEXP_INTERSECT = 
	sig
		type 'a sexp

		val intersect : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end

functor SexpIntersect (
	structure Sexp : SEXP
	structure SexpMemberFunction : SEXP_MEMBER
	sharing type Sexp.sexp = SexpMemberFunction.sexp)
	:> SEXP_INTERSECT where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun intersect (List fst_set) snd_sexp equality_comparer = 
		let
			fun I Null = Null
			|	I (Cons (car_sexp, cdr_slist)) = 
					if SexpMemberFunction.member snd_sexp car_sexp equality_comparer 
					then Cons (car_sexp, I cdr_slist) else I cdr_slist
		in List (I fst_set) end


	end
