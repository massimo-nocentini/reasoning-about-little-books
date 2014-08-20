
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
