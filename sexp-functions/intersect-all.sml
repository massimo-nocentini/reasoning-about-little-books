
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

functor SexpIntersectAllWithLetcc (
	structure Sexp : SEXP
	structure SexpIntersectFunction : SEXP_INTERSECT
	sharing type Sexp.sexp = SexpIntersectFunction.sexp
	structure HopSkipAndJump : HOP_SKIP_AND_JUMP)
	:> SEXP_INTERSECT_ALL where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun intersect_all (empty as List Null) equality_comparer = empty
	|	intersect_all (List list_of_sexps) equality_comparer = 
			HopSkipAndJump.letcc (fn hop =>
				let
					fun	A (Cons (empty as List Null, _)) = hop empty
					|	A (Cons (car_sexp as List _, Null)) = car_sexp
					|	A (Cons (car_sexp as List _, cdr_slist)) = 
							SexpIntersectFunction.intersect car_sexp (A cdr_slist) equality_comparer
				in A list_of_sexps end)


	end

functor SexpIntersectAllWithLetccIntersectEmbedded (
	structure Sexp : SEXP
	structure SexpMemberFunction : SEXP_MEMBER
	sharing type Sexp.sexp = SexpMemberFunction.sexp
	structure HopSkipAndJump : HOP_SKIP_AND_JUMP)
	:> SEXP_INTERSECT_ALL where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun intersect_all (empty as List Null) equality_comparer = empty
	|	intersect_all (List list_of_sexps) equality_comparer = 
			HopSkipAndJump.letcc (fn hop =>
				let

					fun I (List _) (empty as List Null) equality_comparer = hop empty
					|	I (List fst_set) snd_sexp equality_comparer = 
						let
							fun J Null = Null
							|	J (Cons (car_sexp, cdr_slist)) = 
									if SexpMemberFunction.member snd_sexp car_sexp equality_comparer 
									then Cons (car_sexp, J cdr_slist) else J cdr_slist
						in List (J fst_set) end

					fun	A (Cons (empty as List Null, _)) = hop empty
					|	A (Cons (car_sexp as List _, Null)) = car_sexp
					|	A (Cons (car_sexp as List _, cdr_slist)) = 
							I car_sexp (A cdr_slist) equality_comparer

				in A list_of_sexps end)


	end
