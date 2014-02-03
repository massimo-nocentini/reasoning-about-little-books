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
	    the current `list' in the list of sexp that `L'
	    received. *)
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

 
    end
