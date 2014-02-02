functor SexpFunctionsStandardImpl (structure Sexp: SEXP)
	:> SEXP_FUNCTIONS where type 'a sexp = 'a Sexp.sexp
	=
	struct

	(* The functor type checks correctly even if the following two
	 type definitions are omitted.*)
	type 'a sexp = 'a Sexp.sexp
	type 'a slist = 'a Sexp.slist

	open Sexp

	fun to_string toStringFun aSexp =
	    let 
		fun sexp_to_string (Atom anAtom) = toStringFun anAtom
		  | sexp_to_string (List aList) = "(" ^ (slist_to_string aList) ^ ")"
		and slist_to_string Null = ""
		  | slist_to_string (Cons (aSexp, Null)) = sexp_to_string aSexp
		  | slist_to_string (Cons (aSexp, aList)) = 
		    (sexp_to_string aSexp) ^ " " ^ (slist_to_string aList)
	    in
		sexp_to_string aSexp
	    end

	fun is_atom (Atom _) = true
	  | is_atom _ = false

	fun equal eq_fn fst_sexp snd_sexp = 
	    let
		fun equal_sexp (Atom a) (Atom b) = eq_fn a b
		  | equal_sexp (List fst_list) (List snd_list) = 
		    equal_slist fst_list snd_list
		  | equal_sexp _ _ = false
		and equal_slist Null Null = true
		  | equal_slist (Cons (fst_cons_sexp, fst_cons_slist))
				(Cons (snd_cons_sexp, snd_cons_slist)) = 
		    (equal_sexp fst_cons_sexp snd_cons_sexp) andalso
		    (equal_slist fst_cons_slist snd_cons_slist)
		  | equal_slist _ _ = false
	    in
		equal_sexp fst_sexp snd_sexp
	    end

	(* This version of combine_sexp is a little more general
    respect the one presented in ``The Little MLer'' it consumes a
    strategy for combining two slist as first argument and make some
    adjustment the following two arguments of type 'a sexp. It
    eventually call the given strategy in order to build the final
    List sexp. *)
	fun combine combine_slist =
	    let 
		fun put_atom_into_empty_list atom = 
		    (List (Cons (atom, Null)))

		fun C aSexp (atom as Atom _) = 
		    C aSexp (put_atom_into_empty_list atom)
		  | C (atom as Atom _) aSexp = 
		    C (put_atom_into_empty_list atom) aSexp
		  | C (List fst_slist) (List snd_slist) =
		    List (combine_slist fst_slist snd_slist)

	    in C end

	(* The following strategy follow the curry-technique, that is
     after consuming an argument, it returns a function that consumes
     the second argument and that returns the combination of the two
     argument. The important point to observe is that, when only the
     first argument is supplied, the following function only sees the
     first Cons (_, _) since, by the curry style, this application
     produces another function, hence the computation is
     ``suspended'', not exploring the structure of the first argument
     beyond the first Cons.*)
	fun combine_slists_curried Null another_slist = another_slist
	  | combine_slists_curried (Cons (aSexp, slist)) another_slist = 
	    Cons (aSexp, combine_slists_curried slist another_slist)

	(* The following strategy follow the stage-technique, that is
    instead of consuming two arguments in curry-style, it consumes
    only one, allowing to recur on that argument and thus
    ``exploring'' its structure via pattern-matching. Only when the
    sexps in the first argument (and only those at the very first
    level) have been explored, an identity function is produced, in
    order to return the second argument as it is. On the other hand,
    for each Cons encountered, we introduce a function ``make_cons''
    which capture the action of consing each sexp, allowing to recur
    on the first argument since the second argument of ``make_cons''
    has to be evaluated before applying ``make_cons'' body. It is
    important to note how this strategy mimic the curried one, because
    in case of null returns the identity function, otherwise in case
    of a Cons (_, _) it use an helper function which ``simulate'' the
    consing as done in the curried version.*)
	fun combine_slists_staged Null = (fn anotherList => anotherList)
	  | combine_slists_staged (Cons (aSexp, aList)) = 
	    make_cons aSexp (combine_slists_staged aList)
	and make_cons aSexp stage = 
	    (fn anotherList => Cons (aSexp, stage anotherList))
		

	fun two_in_a_row eq_fn sexp = 
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
		S sexp
	    end




	    


	end
