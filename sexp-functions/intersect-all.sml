
signature SEXP_INTERSECT_ALL = 
	sig
		type 'a sexp

		val intersect_all : 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end

functor SexpIntersectAll (
	structure Sexp : SEXP
	structure SexpIntersectFunction : SEXP_INTERSECT
	sharing type Sexp.sexp = SexpIntersectFunction.sexp)
	:> SEXP_INTERSECT_ALL where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun intersect_all (empty as List Null) equality_comparer = empty
	|	intersect_all (List list_of_sexps) equality_comparer = 
		let
			fun A (Cons (car_sexp as List _, Null)) = car_sexp
			|	A (Cons (car_sexp as List _, cdr_slist)) = 
					SexpIntersectFunction.intersect car_sexp (A cdr_slist) equality_comparer
		in A list_of_sexps end


	end
