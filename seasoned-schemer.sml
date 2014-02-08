functor SeasonedSchemer (structure SexpStr : SEXP) = 
    struct

    structure SexpParser = SExpParserSMLofNJ (
	structure aSexp = SexpStr)

    structure SexpFunctions = SexpFunctionsStandardImpl (
	structure Sexp = SexpStr)

    open SexpStr SexpParser SexpFunctions

    fun two_in_a_row_using_helper_function eq_fn = 
	let
	    (* ``is_first_in'' may respond with `false' for two
		 different situation: it returns `false' when the list
		 is empty or when the first element in the list is
		 different from sexp*)
	    fun is_first_in sexp (Cons (another_sexp, _)) = 
		equal eq_fn sexp another_sexp
	      | is_first_in _ Null = false

	    (* If `is_first_in' responds `false' it makes sense
		for `L' to continue the search only if `cdr_list'
		isn't empty*)
	    fun S (Atom _) = false
	      | S (List slist) = L slist
	    and L Null = false
	      | L (Cons (car_sexp, cdr_slist)) = 
		is_first_in car_sexp cdr_slist orelse 
		L cdr_slist
	in
	    S
	end

    (* Write a new version where `two_in_a_row' leaves the decision of
     whether continuing the search is useful to the revised version of
     `is_first_in'.*)
    fun two_in_a_row_leaving_recursion_to_helper eq_fn = 
	let

	    fun S (Atom _) = false
	      | S (List slist) = L slist
	    and L Null = false
	      | L (Cons (car_sexp, cdr_slist)) = 
		is_first_in car_sexp cdr_slist 
	    and is_first_in sexp (list as Cons (another_sexp, _)) = 
		(* if `list' contains at least one atom and if the
		 atom is not the same as `a', we must search for two
		 atoms in a row in `list'. And that's the job of
		 `L'.*)
		equal eq_fn sexp another_sexp orelse L list
	      | is_first_in _ Null = false

	in
	    S
	end

    (* When `is_first_in' determines the value of `L list',
    `two_in_a_row' will request the value of `is_first_in car_sexp
    cdr_slist' since `list' isn't empty. This does mean that we could
    write a function like `is_first_in' that doesn't use `L' at all*)
    fun two_in_a_row_recursion_only_through_helper eq_fn = 
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
	    fun is_first_in preceding (list as Cons (car_sexp, cdr_slist)) = 
		equal eq_fn preceding car_sexp orelse 
		is_first_in car_sexp cdr_slist
	      | is_first_in _ Null = false

	    fun S (Atom _) = false
	      | S (List slist) = L slist
	    and L Null = false
	      | L (Cons (car_sexp, cdr_slist)) = 
		is_first_in car_sexp cdr_slist 
	    
	in
	    S
	end


    fun sum_of_prefixes_helper sonssf Null = Null
      | sum_of_prefixes_helper sonssf (Cons (Atom anInt, cdr)) = 
	(* the answer should be the sum of all the numbers that we
	have seen so far consed onto the natural recursion. So we've
	applied the trick just saw, helping a lot: that trick consists
	of receiving two arguments and one tells it something about
	the other. The argument `SumOfNumberSeenSoFar' is what it namely say@! *)
	Cons (Atom (sonssf + anInt), 
	      sum_of_prefixes_helper (sonssf + anInt) cdr)

    fun sum_of_prefixes_unprotected (List aList) = 
	List (sum_of_prefixes_helper 0 aList)
      | sum_of_prefixes_unprotected (atom as Atom _) = 
	List (Cons (atom, Null))

    (* **************************************************************** *)
    (* ELEVENTH COMMANDMENT: Use additional arguments when a function
     needs to know what other arguments to the function have been like
     so far. *)
    (* **************************************************************** *)

    (* the function `scramble_unprotected' takes a non-empty flat sexp
     (tup for short) of integers in which no number is greater than
     its own index, and returns a tup of the same length. Each number
     in the argument is trated as a backward index from its own
     position to a point earlier in the tup. The result at each
     position is found by counting backward from the current position
     according to this index.*)
    fun scramble_unprotected (List conses) = List (scramble_helper Null conses)
    and scramble_helper reversed_prefix (Cons (car_sexp as Atom anIndex, cdr_slist)) = 
	let
	    val reversed_prefix' = Cons (car_sexp, reversed_prefix)
	    val picked = pick anIndex (List reversed_prefix')
	    val rest = scramble_helper reversed_prefix' cdr_slist
	in Cons (picked, rest) end
      | scramble_helper _ Null = Null
 
    end
