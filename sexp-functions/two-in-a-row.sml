
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

functor SexpTwoInARowLeavingRecursionToHelper(
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp )
	:> SEXP_TWO_IN_A_ROW where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
		open Sexp

		(* Write a new version where `two_in_a_row' leaves the decision of
		 whether continuing the search is useful to the revised version of
		 `is_first_in'.*)
		fun two_in_a_row sexp comparer = 
		let

			fun S (Atom _) = false
			|	S (List slist) = L slist
			and L Null = false
			|	L (Cons (car_sexp, cdr_slist)) = 
			  	is_first_in car_sexp cdr_slist 
			and is_first_in _ Null = false
			|	is_first_in sexp (list as Cons (another_sexp, _)) = 
				(* if `list' contains at least one atom and if the
				 atom is not the same as `a', we must search for two
				 atoms in a row in `list'. And that's the job of
				 `L'.*)
				SexpEqualFunction.equal comparer sexp another_sexp orelse L list

		in
			S sexp
		end

	end

functor SexpTwoInARowRecursionOnlyThroughHelper(
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp )
	:> SEXP_TWO_IN_A_ROW where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
		open Sexp

		(* When `is_first_in' determines the value of `L list',
		`two_in_a_row' will request the value of `is_first_in car_sexp
		cdr_slist' since `list' isn't empty. This does mean that we could
		write a function like `is_first_in' that doesn't use `L' at all*)
		fun two_in_a_row sexp comparer = 
			let
				(* the natural recursion is `is_first_in car_sexp
				cdr_slist', but is quite unusual since both arguments
				change even though the function asks questions about its
				second argument only. The first argument `preceding'
				changes all the time because, as the name of the argument
				says, the first argument is always the atom that precedes
				the current `list' in the list of sexp that `L' received.
				Here the `preceding' argument always occurs just before 
				the second argument, `list', in the original list.*)
				fun is_first_in _ 			Null = false
				|	is_first_in preceding 	(Cons (car_sexp, cdr_slist)) = 
					SexpEqualFunction.equal comparer preceding car_sexp orelse 
						is_first_in car_sexp cdr_slist

				fun S (Atom _) = false
				|	S (List slist) = L slist
				and L Null = false
				|	L (Cons (car_sexp, cdr_slist)) = is_first_in car_sexp cdr_slist 
				
			in
				S sexp
			end

	end


functor SexpTwoInARowStar(
	structure Sexp : SEXP)
	:> SEXP_TWO_IN_A_ROW where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
    open Sexp
    open SMLofNJ.Cont

    (* When `is_first_in' determines the value of `L list',
    `two_in_a_row' will request the value of `is_first_in car_sexp
    cdr_slist' since `list' isn't empty. This does mean that we could
    write a function like `is_first_in' that doesn't use `L' at all*)
    fun two_in_a_row (List slist) comparer =
        let
            val fill_ref = ref NONE
            val leave_ref = ref NONE 
        
            val get_next = fn () => callcc (fn here_again => (
                                        leave_ref := SOME here_again; 
                                        throw (valOf (!fill_ref)) ()))

            fun waddle Null = NONE
            |   waddle (Cons ((Atom a), cdr_slist)) =
                    let val _ = callcc (fn rest => (fill_ref := SOME rest; throw (valOf (!leave_ref)) (SOME a)))
                    in waddle cdr_slist end
            |   waddle (Cons (List car_slist, cdr_slist)) = (waddle car_slist; waddle cdr_slist)

            fun T a = let val n = get_next () 
                            in  case n 
                                of  NONE => false
                                |   SOME a' => comparer a a' orelse T a' end

            val heading = fn () =>  let val fst = callcc (
                                            fn here => (leave_ref := SOME here; 
                                                        waddle slist;
                                                        throw (valOf (!leave_ref)) NONE))
                                    in case fst
                                        of  NONE => false
                                        |   SOME atom => T atom end 
                
        
        
        in heading ()  end 

    end





















