
signature SEXP_MULTIREMBER = 
	sig
		type 'a sexp

		val multirember : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end

	(*
	 TODO: after the Y combinator finds its home in a signature,
	 write a functor that uses it to define a structure that implements
	 SEXP_MULTIREMBER.
	*)

functor SexpMultiremberNaive (
	structure Sexp : SEXP
	structure SexpEqualFunction : SEXP_EQUAL
	sharing type Sexp.sexp = SexpEqualFunction.sexp)
	:> SEXP_MULTIREMBER where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
		open Sexp

		fun multirember aSexp anElement equality_comparer = 
			let 
				(* The definition of `multirember_sexp' refers to
				`anElement' which stands for the atom that we need to
				remove from `aSexp'. A function defined in this `naming
				part' (corresponding to (letrec ...) naming part) knows
				all the arguments of all the surrounding `fun's
				expressions.*)
				fun multirember_sexp (List conses) = List (multirember_slist conses)
				and multirember_slist Null = Null
				|	multirember_slist (Cons (car_sexp, cdr_slist)) = 
					if SexpEqualFunction.equal equality_comparer car_sexp anElement 
					then multirember_slist cdr_slist
					else Cons (car_sexp, multirember_slist cdr_slist)
			in multirember_sexp aSexp end

	end
