
signature SEXP_UNION = 
	sig
		type 'a sexp

		val union : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end

functor SexpUnion (
	structure Sexp : SEXP
	structure SexpMemberFunction : SEXP_MEMBER
	sharing type Sexp.sexp = SexpMemberFunction.sexp)
	:> SEXP_UNION where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun union (List fst_set) (snd_sexp as List snd_set) equality_comparer = 
		let
			fun U Null = snd_set
			|	U (Cons (car_sexp, cdr_slist)) = 
					if SexpMemberFunction.member snd_sexp car_sexp equality_comparer 
					then U cdr_slist else Cons (car_sexp, U cdr_slist)
		in List (U fst_set) end


	end
