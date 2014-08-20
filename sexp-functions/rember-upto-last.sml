
signature SEXP_REMBER_UPTO_LAST = 
	sig
		type 'a sexp

		val rember_upto_last : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end

(*
 Can you describe in two sentences what `rember_upto_last' does?
 "Function `rember_upto_last' takes an atom `a' and a lat and removes
 all the atoms from the lat upto and including the last occurrences of
 atom `a'. If there are no occurences of `a', the function simply returns
 the original lat (aka: list of atoms). So, upon discovering the atom `a', 
 the new version would not stop looking at elements in `lat' but would also
 throw away everything it had seen so far. You mean it would forget some
 computation that it had remembered somewhere? Yes, it would. Does it sound
 like `intersect_all'? It sounds like it: it knows that the first few
 atoms do not contribute to the final result. But then again it sounds
 different, too. Different in what sense? The function `intersect_all' 
 knows what the result is; function `rember_upto_last' knows which pieces
 of the list are not in the result. But does it know where it can find the
 result? The result is the `rember_upto_last' of the rest of the list. Suppose
 `rember_upto_last' sees the atom `a', should it forget the pending computations
 and should it restart the process of searching through the rest of the list?
 Yes, it should.
*)
functor SexpRemberUptoLast (
	structure Sexp : SEXP
	structure SexpEqualFunction : SEXP_EQUAL
	sharing type Sexp.sexp = SexpEqualFunction.sexp
	structure HopSkipAndJump : HOP_SKIP_AND_JUMP)
	:> SEXP_REMBER_UPTO_LAST where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun rember_upto_last (List slist) target_sexp comparer = 
		HopSkipAndJump.letcc (fn skip =>
			let
				fun R Null = Null
				|	R (Cons (car_sexp, cdr_slist)) = 
						if SexpEqualFunction.equal comparer car_sexp target_sexp
						then skip (List (R cdr_slist))
						else Cons (car_sexp, R cdr_slist)	
			in List (R slist) end
		)

	end
