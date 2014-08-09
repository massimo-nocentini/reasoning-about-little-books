
signature SEXP_SUM_OF_PREFIXES = 
	sig
		type 'a sexp

		(*
		 It should be nice to abstract the sum and implement something like
		 an #inject:into: function (or fold in functional programming terms).
		*)
		val sum_of_prefixes : int sexp -> int sexp
	end

functor SexpSumOfPrefixes (
	structure Sexp : SEXP)
	:> SEXP_SUM_OF_PREFIXES where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
		open Sexp

		fun sum_of_prefixes_helper sonssf Null = Null
		|	sum_of_prefixes_helper sonssf (Cons (Atom anInt, cdr)) = 
			(* the answer should be the sum of all the numbers that we
			have seen so far consed onto the natural recursion. So we've
			applied the trick just saw, helping a lot: that trick consists
			of receiving two arguments and one tells it something about
			the other. The argument `SumOfNumberSeenSoFar' is what it
			namely say@! *)
			Cons (Atom (sonssf + anInt), 
				  sum_of_prefixes_helper (sonssf + anInt) cdr)

		fun sum_of_prefixes_unprotected (List aList) = 
			List (sum_of_prefixes_helper 0 aList)
		|	sum_of_prefixes_unprotected (atom as Atom _) = 
			List (Cons (atom, Null))

		val sum_of_prefixes = sum_of_prefixes_unprotected

	end

