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

 
    end
