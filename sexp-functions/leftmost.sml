
signature SEXP_LEFTMOST = 
	sig
		type 'a sexp

		val leftmost : 'a sexp -> 'a sexp
	end


functor SexpLeftmost (
	structure Sexp : SEXP)
	:> SEXP_LEFTMOST where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	(*
	 The following implementation looks pretty different 
	 from the one given in the book, in particular we do not
	 use `let' in an explicit form, but the `case' perform
	 the computation once and `atom as Atom _' gives a name.
	 *)
	fun	leftmost (atom as Atom _) = atom
	|	leftmost (List slist) = leftmost_slist slist
	and leftmost_slist (empty as Null) = List empty
	| 	leftmost_slist (Cons (car_sexp, cdr_slist)) =
			let val something = leftmost car_sexp
			in	case something of 
					Atom _ 	=> something
				|	_		=> leftmost_slist cdr_slist
			end
		(* The following is a more compact version:
			case leftmost car_sexp of
				atom as Atom _ => atom
			|	_ => leftmost_slist cdr_slist
		*)
	end

(*
 Previous implementation, when used on the list `(((a)) b (c))'
 recur down on the left finding atom 'a but after it finds it, leftmost
 has to give it a name and check if it is an atom , which is the case, for 
 three times, that is it does this checks for the natural recursions performed.
 Have we been here before? Yes, we have. When we discussed `intersect_all', we 
 also discovered that we really had the final answer long before we could say so.
 And what did we do then? We used `letcc'. The function `leftmost' sets up a
 North Pole in `skip' and then determines the value of `leftmost_sexp'
 consuming the given sexp. This function looks at every atom in the sexp
 from left to right until it finds an atom and then uses `skip' to return
 this atom adruptly and promptly, forgetting all the things (ie: checking 
     if it is really an atom) it remembers to do and resume its work with 
 `letcc skip a', where a is 'a.
 *)
functor SexpLeftmostWithLetcc (
	structure Sexp : SEXP
	structure HopSkipAndJump : HOP_SKIP_AND_JUMP)
	:> SEXP_LEFTMOST where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun leftmost sexp = HopSkipAndJump.letcc (fn skip =>
		let
			fun	leftmost_sexp (atom as Atom _) = skip atom
			|	leftmost_sexp (List slist) = leftmost_slist slist
			and leftmost_slist (empty as Null) = List empty
			| 	leftmost_slist (Cons (car_sexp, cdr_slist)) =
					let 
						val _ 			= leftmost_sexp car_sexp
						val something 	= leftmost_slist cdr_slist
					in something end
		in leftmost_sexp sexp end
	)

	end
