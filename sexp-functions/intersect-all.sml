
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
	
	(*
	 The first line allow to make no assumptions about the list of sets,
	 as in `The Little Schemer' we assumed that that list wasn't empty.
	 Since this question is just to make sure that the list of sexps isn't 
	 empty, once we know it is the case we never have to ask the question
	 again, hence we hide the `normal' behavior in the function `A'.
	 *)
	fun intersect_all (empty as List Null) equality_comparer = empty
	|	intersect_all (List list_of_sexps) equality_comparer = 
		let
			fun A (Cons (car_sexp as List _, Null)) = car_sexp
			|	A (Cons (car_sexp as List _, cdr_slist)) = 
					SexpIntersectFunction.intersect car_sexp (A cdr_slist) equality_comparer
		in A list_of_sexps end


	end

(*
 When there is an empty set in the list of sets, previous `intersect_all'
 function returns the empty set. But this does not show how
 `intersect_all' determines that the intersection is empty.
 No, it does not. Instead, it keeps intersecting the empty set with
 some more set until the list of sets is exhausted. Wouldn't be better
 if `intersect_all' didn't have to intersect each set with the empty
 set and if it could say "This is it: the result is () and that's
 all there is to it". That would be an improvement. It could save us a
 lot of work if we need to determine the result of `intersect_all'.
 *)
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
					(*
					 A will "hop" to the right place when one of
					 the sets is empty. This does mean: "Forget what
					 we had remembered to do after leaving behind
					 `letcc' and before encountering `hop M' and
					 then act as if we were to determine the value of
					 `letcc M', whatever M is." But how do we forget
					 something? Easy, we do not do it! Hence we do not
					 intersect the pending sets with the result of the
					 natural recursion.
					 *)
					fun	A (Cons (empty as List Null, _)) = hop empty
					|	A (Cons (car_sexp as List _, Null)) = car_sexp
					|	A (Cons (car_sexp as List _, cdr_slist)) = 
							SexpIntersectFunction.intersect car_sexp (A cdr_slist) equality_comparer
				in A list_of_sexps end)


	end

(*
 When `intersect set1 set2' produces the empty set, with the above
 `intersect_all' implementation we need to intersect the empty set
 several times with a set, before we could say that the result of 
 intersect_all was the empty set. Is it a mistake of
 `intersect_all'? Yes, and it is also a mistake of `intersect'. In
 what sense? We could have defined `intersect' so that it would not
 do anything when its second argument is the empty set. Why its
 second argument? When the first sexp (set) is finally empty,
 it could be because it is always empty or because `intersect'
 has looked at all of its arguments. But when the second sexp
 (set) is empty, `intersect' should not look at any elements in
 the first sexp (set) at all! It knows the result!
 *)
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

					(*
					 We define an extra question about the second
					 sexp, but the answer isn't simply `empty' since
					 it would immediately return the correct result
					 but this still does not work right with
					 `intersect_all'. Why not? When one of the
					 intersects produces () in `intersect_all', we
					 know the result of `intersect_all'. Well, we
					 could build in a question that looks at the
					 result of `interset' and "hops" if necessary? But
					 somehow that looks wrong. Why wrong? Because
					 `intersect' asks this very same question. We
					 would just duplicate it. Got it. You mean that we
					 should have a version of `intersect' that "hops"
					 all the way over all the intersects in
					 `intersect_all'? Yes, that would be great, and we
					 can use `hop' even in `intersect' if we want to
					 jump and to return the empty set without further
					 delay, because `hop' is like a compass needle and
					 it is attracted to the North Pole, letcc (fn hop
					*)
					fun I (List _) (empty as List Null) = hop empty
					|	I (List fst_set) snd_sexp = 
						let
							fun J Null = Null
							|	J (Cons (car_sexp, cdr_slist)) = 
									if SexpMemberFunction.member snd_sexp car_sexp equality_comparer 
									then Cons (car_sexp, J cdr_slist) else J cdr_slist
						in List (J fst_set) end

					fun	A (Cons (empty as List Null, _)) = hop empty
					|	A (Cons (car_sexp as List _, Null)) = car_sexp
					|	A (Cons (car_sexp as List _, cdr_slist)) = 
							I car_sexp (A cdr_slist) 

				in A list_of_sexps end)


	end
