
signature SEXP_TWO_IN_A_ROW = 
    sig
        type 'a sexp

		(* 
		 it should be interesting to remove the comparer function
		 in order to have a more clear type for two_in_a_row
		*)
        val two_in_a_row : 'a sexp -> ('a -> 'a -> bool) -> bool
    end
(*
 The following is an attempt to remove the comparer function
 from the signature: it does compile but with the current 
 structure we cannot supply a structure that type checks
 the proposed requirements.
*)
(*
functor SexpTwoInARowWithIndependentHelper(
	type elem
	type 'a t
	structure Sexp : SEXP where type 'a sexp =elem t 
	structure SexpEqualFunction : SEXP_EQUAL 
		where type 'a sexp = 'a Sexp.sexp
	val comparer : elem -> elem -> bool) 
	:> SEXP_TWO_IN_A_ROW where type 'a sexp = 'a Sexp.sexp
*)
functor SexpTwoInARowWithIndependentHelper(
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp )
	:> SEXP_TWO_IN_A_ROW where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
		open Sexp

		fun two_in_a_row sexp comparer = 
			let
				(* ``is_first_in'' may respond with `false' for two
				 different situation: it returns `false' when the list
				 is empty or when the first element in the list is
				 different from sexp*)
				fun is_first_in _ Null = false
				|	is_first_in sexp (Cons (another_sexp, _)) = 
					SexpEqualFunction.equal comparer sexp another_sexp

				(* If `is_first_in' responds `false' it makes sense
				for `two_in_a_row_slist' to continue the search only if `cdr_list'
				isn't empty*)
				fun two_in_a_row_sexp (Atom _) = false
				  | two_in_a_row_sexp (List slist) = two_in_a_row_slist slist
				and two_in_a_row_slist Null = false
				  | two_in_a_row_slist (Cons (car_sexp, cdr_slist)) = 
					is_first_in car_sexp cdr_slist orelse two_in_a_row_slist cdr_slist

			in two_in_a_row_sexp sexp end

	end
