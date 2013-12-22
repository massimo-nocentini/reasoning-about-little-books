structure LittleSchemer = 
    struct

    open SExpressions

    fun occurs_in_slist x Null = 0
      | occurs_in_slist x (Cons (sexp, aList)) = 
	(occurs_in_sexp x sexp) + (occurs_in_slist x aList)
    and occurs_in_sexp x (Atom y) = 
	if x = y then 1 else 0
      | occurs_in_sexp x (List aList) =
	occurs_in_slist x aList

    fun subst_in_slist new old Null = Null
      | subst_in_slist new old (Cons (sexp, slist)) = 
	Cons ((subst_in_sexp new old sexp), 
	      (subst_in_slist new old slist))
    and subst_in_sexp new old (original as (Atom atom)) =
	if atom = old then Atom new else original
      | subst_in_sexp new old (List aList) =
	List (subst_in_slist new old aList) 

    fun remove_from_sexp pred = 
	let 
	    fun R_from_slist Null = Null
	      | R_from_slist (Cons (sexp, slist)) =
		let
		    fun first_sexp_is_atom_to_be_removed (Atom a) =  pred a
		      | first_sexp_is_atom_to_be_removed (List _) = false
		in 
		    if first_sexp_is_atom_to_be_removed sexp 
		    then R_from_slist slist
		    else Cons ((R_from_sexp sexp), 
			       (R_from_slist slist))
		end
	    and R_from_sexp (original as Atom a) = original
	      | R_from_sexp (List slist) = 
		List (R_from_slist slist)
	in
	    R_from_sexp
	end

    exception RemoveCannotBeAppliedToAtomicSexp

    fun remove_from_sexp_abridged pred (List slist)= 
	let 
	    fun R_from_slist Null = Null
	      | R_from_slist (Cons ((atom as Atom a), alist)) = 
		if pred a
		then R_from_slist alist
		else Cons (atom, R_from_slist alist)
	      | R_from_slist (Cons ((List fsList), snList)) = 
		Cons ((List (R_from_slist fsList)), (R_from_slist snList))
	in
	    List (R_from_slist slist)
	end
      | remove_from_sexp_abridged _ _ = raise RemoveCannotBeAppliedToAtomicSexp

    (* The function ``remove_from_sexp_abridged_toward_Y_step_1'' is
    like the function ``remove_from_sexp_abridged'': it takes one
    extra-argument ``hukairs'' and, when it is applied to an argument,
    it produces a function that looks like remove_from_sexp_abridged
    except for the application ``R_from_slist hukairs alist''. *)
    fun remove_from_sexp_abridged_toward_Y_step_1 pred (List slist)= 
	let 
	    (* The parameter ``hukairs'' is just passed around: when
    ``R_from_list'' reaches the leafs of the sexp, ``hukairs'' is not used. *)
	    fun R_from_slist hukairs Null = Null
	      | R_from_slist hukairs (Cons ((atom as Atom a), alist)) = 
		if pred a 
		then R_from_slist hukairs alist
		else Cons (atom, R_from_slist hukairs alist)
	      | R_from_slist hukairs (Cons ((List fsList), snList)) = 
		Cons ((List (R_from_slist hukairs fsList)), 
		      (R_from_slist hukairs snList))
	in
	    (* Can ``remove_from_sexp_abridged_toward_Y_step_1''
    define ``remove_from_sexp_abridged''? Yes, and doesn't matter what
    we use to define remove_from_sexp_abridged, since ``hukairs'' is
    never used.*)
	    List (R_from_slist 0 slist)
	end
      | remove_from_sexp_abridged_toward_Y_step_1 _ _ = 
	raise RemoveCannotBeAppliedToAtomicSexp

    fun remove_from_sexp_abridged_toward_Y_step_2 pred (List slist)= 
	let 
	    fun R_from_slist hukairs Null = Null
	      | R_from_slist hukairs (Cons ((atom as Atom a), alist)) = 
		if pred a   
		then R_from_slist hukairs alist
		else Cons (atom, R_from_slist hukairs alist)
	      | R_from_slist hukairs (Cons ((List fsList), snList)) = 
		Cons ((List (R_from_slist hukairs fsList)), 
		      (R_from_slist hukairs snList))
	in
	    (* Can we use ``R_from_slist'' to define
	remove_from_sexp_abridged_toward_Y_step_1 giving
	``R_from_slist''? In this way the parameter ``hukairs'' become
	``R_from_list''!*)
	    List (R_from_slist R_from_slist slist)
	end
      | remove_from_sexp_abridged_toward_Y_step_2 _ _ = 
	raise RemoveCannotBeAppliedToAtomicSexp

    (* fun remove_from_sexp_abridged_toward_Y_step_3 pred (List slist)=  *)
    (* 	let  *)
    (* 	    fun R_from_slist hukairs Null = Null *)
    (* 	      | R_from_slist hukairs (Cons ((atom as Atom a), alist)) =  *)
    (* 		if pred a    *)
    (* 		then hukairs hukairs alist *)
    (* 		else Cons (atom, hukairs hukairs alist) *)
    (* 	      | R_from_slist hukairs (Cons ((List fsList), snList)) =  *)
    (* 		Cons ((List (hukairs hukairs fsList)),  *)
    (* 		      (hukairs hukairs snList)) *)
    (* 	in *)
    (* 	    (* Can we use ``R_from_slist'' to define *)
    (* 	remove_from_sexp_abridged_toward_Y_step_1 giving *)
    (* 	``R_from_slist''? In this way the parameter ``hukairs'' become *)
    (* 	``R_from_list''!*) *)
    (* 	    List (R_from_slist R_from_slist slist) *)
    (* 	end *)
    (*   | remove_from_sexp_abridged_toward_Y_step_3 _ _ =  *)
    (* 	raise RemoveCannotBeAppliedToAtomicSexp *)

    datatype 'a T = Into of 'a T -> 'a

    fun Y f = H f (Into (H f))
    (* and H f into = f (G into) *)
    and H f = f o G
    and G (into as Into aFn) x = aFn into x
    (* and G (into as Into aFn) = aFn into *)

    val factorial = 
	let
	    fun mkfact fact 0 = 1
	      | mkfact fact n = n * fact (n-1)
	in
	    Y mkfact
	end

    end
