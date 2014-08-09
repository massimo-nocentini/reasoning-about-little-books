
signature SEXP_SCRAMBLE = 
	sig
		type 'a sexp

		val scramble : int sexp -> int sexp
	end

functor SexpScrambleUnprotected (
	structure Sexp : SEXP
	structure SexpPickFunction : SEXP_PICK
	sharing type Sexp.sexp = SexpPickFunction.sexp)
	:> SEXP_SCRAMBLE where type 'a sexp = 'a Sexp.sexp
	=
	struct

		open Sexp

		fun scramble_helper _ Null = Null
		|	scramble_helper reversed_prefix (Cons (car_sexp as Atom anIndex, cdr_slist)) = 
			let
				val reversed_prefix' = Cons (car_sexp, reversed_prefix)
				val picked = SexpPickFunction.pick (List reversed_prefix') anIndex 
				val rest = scramble_helper reversed_prefix' cdr_slist
			in Cons (picked, rest) end
		   
		(* the function `scramble_unprotected' takes a non-empty flat sexp
		 (tup for short) of integers in which no number is greater than
		 its own index, and returns a tup of the same length. Each number
		 in the argument is treated as a backward index from its own
		 position to a point earlier in the tup. The result at each
		 position is found by counting backward from the current position
		 according to this index.*)
		fun scramble (List conses) = List (scramble_helper Null conses)

	end
