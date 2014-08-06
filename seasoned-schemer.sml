functor SeasonedSchemer (structure SexpStr : SEXP) = 
    struct

    structure SexpParser = SExpParserSMLofNJ (
	structure aSexp = SexpStr) 

    structure SexpFunctions = SexpFunctionsStandardImpl (
	structure Sexp = SexpStr)

    open SexpStr 
    open SexpParser 
    open SexpFunctions

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
	the other. The argument `SumOfNumberSeenSoFar' is what it
	namely say@! *)
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
     in the argument is treated as a backward index from its own
     position to a point earlier in the tup. The result at each
     position is found by counting backward from the current position
     according to this index.*)
    fun scramble_unprotected (List conses) = 
	List (scramble_helper Null conses)
    and scramble_helper reversed_prefix 
			(Cons (car_sexp as Atom anIndex, cdr_slist)) = 
	let
	    val reversed_prefix' = Cons (car_sexp, reversed_prefix)
	    val picked = pick anIndex (List reversed_prefix')
	    val rest = scramble_helper reversed_prefix' cdr_slist
	in Cons (picked, rest) end
      | scramble_helper _ Null = Null
 
    fun multirember anElement aSexp = 
	let 
	    fun curried_equal a b = a = b

	    (* The definition of `multirember_sexp' refers to
	    `anElement' which stands for the atom that we need to
	    remove from `aSexp'. A function defined in this `naming
	    part' (corresponding to (letrec ...) naming part) knows
	    all the arguments of all the surrounding `fun's
	    expressions.*)
	    fun multirember_sexp (List conses) = 
		List (multirember_slist conses)
	    and multirember_slist Null = Null
	      | multirember_slist (Cons (car_sexp, cdr_slist)) = 
		if equal curried_equal anElement car_sexp 
		then multirember_slist cdr_slist
		else Cons (car_sexp, multirember_slist cdr_slist)
	in multirember_sexp aSexp end

    (* Can you define the function `multirember_with_test' which
    relates to `multirember' consuming a function to abstract test
    logic? The function `multirember_with_test' accepts a function
    `test' and returns a new function, let us call this latter
    function `m_f'. The function `m_f' takes a sexp `anElement' and a
    list of sexps and traverses the latter. Any sexp `b' in the list
    for which `test anElement b' is true, is removed. Is it true that
    during this traversal the result of `multirember_with_test aTest'
    is always the same? Yes, it is always the function for which we
    just used the name `m_f'... *)
    fun multirember_with_test test anElement (List conses) = 
	multirember_with_test_slist test anElement conses
    and multirember_with_test_slist test anElement Null = Null
      | multirember_with_test_slist test anElement (Cons (car_sexp, cdr_slist)) = 
	if test anElement car_sexp 
	then multirember_with_test_slist test anElement cdr_slist
	else Cons (car_sexp, multirember_with_test_slist test anElement cdr_slist)

    (* ...Perhaps `multirember_with_test should name it `m_f' *)
    fun multirember_with_test_slist_abridged_1 test = 
	let
	    fun m_f anElement (List conses) = 
		m_f_slist anElement conses
	    and m_f_slist anElement Null = Null
	      | m_f_slist anElement (Cons (car_sexp, cdr_slist)) = 
		if test anElement car_sexp 
		then m_f_slist anElement cdr_slist
		else Cons (car_sexp, m_f_slist anElement cdr_slist)
	in m_f end

    (* What is the value of `multirember_with_test_slist_abridged_1
    aTest' where `aTest' is the equal comparison among sexps? *)
    val multirember_with_test_slist_abridged_2 = 
	let
	    fun curried_equal a b = a = b

	    fun m_r anElement (List conses) = 
		m_r_slist anElement conses
	    and m_r_slist anElement Null = Null
	      | m_r_slist anElement (Cons (car_sexp, cdr_slist)) = 
		if equal curried_equal anElement car_sexp 
		then m_r_slist anElement cdr_slist
		else Cons (car_sexp, m_r_slist anElement cdr_slist)
	in m_r end

    (* Did you notice that no `fun ...' surrounds the
    `let...in...end'? It looks odd, but is correct! Could we have used
    another name for the function named in `let...in...end'? Yes,
    `m_r' is `multirember_with_test_slist_abridged_2' *)
    val multirember_with_test_slist_abridged_2 = 
	let
	    fun curried_equal a b = a = b

	    fun multirember_with_test_slist_abridged_2 anElement (List conses) = 
		multirember_with_test_slist_abridged_2_slist 
		    anElement conses
	    and multirember_with_test_slist_abridged_2_slist anElement Null = Null
	      | multirember_with_test_slist_abridged_2_slist 
		    anElement (Cons (car_sexp, cdr_slist)) = 
		if equal curried_equal anElement car_sexp 
		then multirember_with_test_slist_abridged_2_slist anElement cdr_slist
		else Cons (car_sexp, 
			   multirember_with_test_slist_abridged_2_slist anElement cdr_slist)
	in multirember_with_test_slist_abridged_2 end

    (* Since `let...in...end' defines a recursive function and since
    `val ... = ...' pairs up names with values, we could eliminate
    `let...in...end' here, right? Yes, we could and we would get back
    our old friend `multirember'. *)
    fun multirember_with_test_slist_abridged_3 anElement aSexp = 
	let
	    fun curried_equal a b = a = b

	    fun multirember_sexp (List conses) = 
		multirember_slist conses
	    and multirember_slist Null = Null
	      | multirember_slist (Cons (car_sexp, cdr_slist)) = 
		if equal curried_equal anElement car_sexp 
		then multirember_slist cdr_slist
		else Cons (car_sexp, multirember_slist cdr_slist)
	in multirember_sexp aSexp end

	

    end
