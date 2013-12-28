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
    except for the application ``curry_maker hukairs alist''. *)
    fun remove_from_sexp_abridged_toward_Y_step_1 pred (List slist)= 
	let 
	    fun curry_maker hukairs =
		fn Null => Null
	      | Cons ((atom as Atom a), alist) =>
		if pred a 
		then curry_maker hukairs alist
		else Cons (atom, curry_maker hukairs alist)
	      | Cons ((List fsList), snList) => 
		Cons ((List (curry_maker hukairs fsList)), 
		      (curry_maker hukairs snList))

	    (* The argument for the parameter ``hukairs'' is just
    passed around: when ``curry_maker'' reaches the leafs of the sexp,
    ``hukairs'' is not used. *)
	    val Mrember_curry = curry_maker 0
	in
	    (* Can ``remove_from_sexp_abridged_toward_Y_step_1''
    define ``remove_from_sexp_abridged''? Yes, and doesn't matter what
    we use to define remove_from_sexp_abridged, since ``hukairs'' is
    never used.*)
	    List (Mrember_curry slist)
	end
      | remove_from_sexp_abridged_toward_Y_step_1 _ _ = 
	raise RemoveCannotBeAppliedToAtomicSexp

    fun remove_from_sexp_abridged_toward_Y_step_2 pred (List slist)= 
	let 
	    fun curry_maker hukairs =
		fn Null => Null
	      | Cons ((atom as Atom a), alist) =>
		if pred a 
		then curry_maker hukairs alist
		else Cons (atom, curry_maker hukairs alist)
	      | Cons ((List fsList), snList) => 
		Cons ((List (curry_maker hukairs fsList)), 
		      (curry_maker hukairs snList))

	    (* Can we use ``curry_maker'' to define Mrember_curry with
	     ``curry_maker''? Of course, and the argument ``hukairs''
	     become ``curry_maker'' itself!*)
	    val Mrember_curry = curry_maker curry_maker
	in
	    List (Mrember_curry slist)
	end
      | remove_from_sexp_abridged_toward_Y_step_2 _ _ = 
	raise RemoveCannotBeAppliedToAtomicSexp

    datatype 'a T = Into of 'a T -> 'a

    fun Y f = H f (Into (H f))
    (* and H f into = f (G into) *)
    and H f = f o G
    and G (into as Into aFn) x = aFn into x
    (* using the following definition the type-checker takes very long
     time to type-check the phrase.*)
    (* and G (into as Into aFn) = aFn into *)

    (* Can we use ``curry_maker'' to define ``Mrember_curry'' with
     ``curry_maker''? Of course, and the argument ``future'' become
     ``curry_maker'' itself! But we can't use use ``future'' to
     replace ``curry_maker'' without getting a type-check error, hence
     we've to build a value of type 'a T using the constructor
     ``Into''.*)
    fun remove_from_sexp_abridged_toward_Y_step_3 pred (List slist)=
    	let
	    (* but can't we use ``future'' to replace ``curry_maker'' 
	     in ``curry_maker''? Yes, we sure can, using the H combinator
	     defined before. In what follows we rename ``curry_maker''
	     in ``function_maker'' since its results are functions.*)
	    fun function_maker (future as (Into _)) =
		fn Null => Null
	      | Cons ((atom as Atom a), alist) =>
		if pred a 
		then G future alist
		else Cons (atom, G future alist)
	      | Cons ((List fsList), snList) => 
		Cons (List (G future fsList), G future snList)

	    (* Write ``Mrember_curry'' using just ``function_maker''... *)
	    val Mrember_curry = function_maker (Into function_maker)
	(* Why does this definition of ``Mrember_curry'' work?
	 Because the value of ``G future'' is the same as
	 ``function_maker (Into function_maker)'' which is the same asp
	 ``Mrember_curry''*)
    	in
	    List (Mrember_curry slist)
    	end
      | remove_from_sexp_abridged_toward_Y_step_3 _ _ =
    	raise RemoveCannotBeAppliedToAtomicSexp

    (* Describe in your own words the function ``function_maker'':
     When the function ``function_maker'' is applied to one argument
     v, such that v is a value built with ``Into'' constructor, and v
     keeps a function that consumes another function and produces
     ``Mrember_curry'', then ``function_maker'' yields Mrember_curry.

     That explanation sounds as if ``function_maker'' needs an
     argument that is just like ``function_maker'' in order to
     construct ``Mrember_curry''*)

    (* Do we have to give a name to ``function_maker''? No, because 
     ``function_maker'' does not appear within its definition.
    
    Do we have to give a name to ``Mrember_curry''? No, because
    ``Mrember_curry'' does not appear within its definition.

    True or false: NO recursive function needs to be given a name
    with ``fun ...''? True, we chose ``Mrember_curry'' as an arbitrary
    recursive function.
     *)
    fun remove_from_sexp_abridged_toward_Y_step_4 pred (List slist)=
    	let
	    (* Is this definition below the same as the 
	     ``function_maker'' we defined in step 3?
	     Yes, because for an arbitrary function f we can always
	     replace f by (fn arg => f arg). *)
	    fun function_maker (future as (Into _)) =
		fn Null => Null
	      | Cons ((atom as Atom a), alist) =>
		if pred a 
		then (fn arg => G future arg) alist
		else Cons (atom, (fn arg => G future arg) alist)
	      | Cons ((List fsList), snList) => 
		Cons (List ((fn arg => G future arg) fsList), 
		      (fn arg => G future arg) snList)

	    val Mrember_curry = function_maker (Into function_maker)
    	in
	    List (Mrember_curry slist)
    	end
      | remove_from_sexp_abridged_toward_Y_step_4 _ _ =
    	raise RemoveCannotBeAppliedToAtomicSexp

    (* Is this definition below the same as the 
     ``function_maker'' we defined in step 4?
     Yes, because the sexp argument does not appear in
     ``(fn arg => G future arg)'', hence we can abstract out
     this piece, replacing it by an atom that is associated with it.
     We chose ``recfun'' as  argument name. *)
    fun remove_from_sexp_abridged_toward_Y_step_5 pred (List slist)=
    	let
	    fun function_maker (future as (Into _)) =
		(fn recfun => 
		   fn Null => Null
		| Cons ((atom as Atom a), alist) =>
		  if pred a 
		  then recfun alist
		  else Cons (atom, recfun alist)
		| Cons ((List fsList), snList) => 
		  Cons (List (recfun fsList), 
			recfun snList)) 
		    (fn arg => G future arg)

	    val Mrember_curry = function_maker (Into function_maker)
    	in
	    List (Mrember_curry slist)
    	end
      | remove_from_sexp_abridged_toward_Y_step_5 _ _ =
    	raise RemoveCannotBeAppliedToAtomicSexp

    (* Can you make the definition of ``function_maker'' simpler by
     breaking it up into two functions? Yes, because it is safe to 
     name the expression ``(fn recfun => ...)'', ie all the variables
     are explicit arguments to M, or they are primitives. *)
    fun remove_from_sexp_abridged_toward_Y_step_6 pred (List slist)=
    	let
	    val M = (fn recfun => 
		   fn Null => Null
		| Cons ((atom as Atom a), alist) =>
		  if pred a 
		  then recfun alist
		  else Cons (atom, recfun alist)
		| Cons ((List fsList), snList) => 
		  Cons (List (recfun fsList), 
			recfun snList)) 

	    fun function_maker (future as (Into _)) =
		M (fn arg => G future arg)

	    val Mrember_curry = function_maker (Into function_maker)
    	in
	    List (Mrember_curry slist)
    	end
      | remove_from_sexp_abridged_toward_Y_step_6 _ _ =
    	raise RemoveCannotBeAppliedToAtomicSexp

    (* Write ``Mrember_curry'' without using ``function_maker''! Hint:
     use the most recent definition of ''function_maker'' in two different places.*)
    fun remove_from_sexp_abridged_toward_Y_step_7 pred (List slist)=
    	let
	    val M = (fn recfun => 
		   fn Null => Null
		| Cons ((atom as Atom a), alist) =>
		  if pred a 
		  then recfun alist
		  else Cons (atom, recfun alist)
		| Cons ((List fsList), snList) => 
		  Cons (List (recfun fsList), 
			recfun snList)) 

	    val function_maker =
	     (fn (future as (Into _)) =>
		M (fn arg => G future arg))

	    val Mrember_curry = 
		(fn (future as (Into _)) => M (fn arg => G future arg)) 
		    (Into (fn (future as (Into _)) =>
			      M (fn arg => G future arg)))
    	in
	    List (Mrember_curry slist)
    	end
      | remove_from_sexp_abridged_toward_Y_step_7 _ _ =
    	raise RemoveCannotBeAppliedToAtomicSexp

    (* Abstract the definition of ``Mrember_curry'' by abstracting away
     the association with ``M''...Hint: wrap a (fn M => ...) around 
     the definition...*)
    fun remove_from_sexp_abridged_toward_Y_step_8 pred (List slist)=
    	let
	    (* M's type: *)
	    (* ('a SExpressions.slist -> 'a SExpressions.slist) ->  *)
	    (* 'a SExpressions.slist -> 'a SExpressions.slist *)
	    val M = (fn recfun => 
		   fn Null => Null
		| Cons ((atom as Atom a), alist) =>
		  if pred a 
		  then recfun alist
		  else Cons (atom, recfun alist)
		| Cons ((List fsList), snList) => 
		  Cons (List (recfun fsList), 
			recfun snList)) 

	    (* Y's type: *)
	    (* (('b -> 'c) -> 'b -> 'c) -> 'b -> 'c *)
	    val Y = fn M => 
		       (fn (future as (Into _)) => M (fn arg => G future arg)) 
			   (Into (fn (future as (Into _)) =>
				     M (fn arg => G future arg)))

	    val Mrember_curry = Y M
    	in
	    List (Mrember_curry slist)
    	end
      | remove_from_sexp_abridged_toward_Y_step_8 _ _ =
    	raise RemoveCannotBeAppliedToAtomicSexp



    val factorial = 
	let
	    fun mkfact fact 0 = 1
	      | mkfact fact n = n * fact (n-1)
	in
	    Y mkfact
	end

    (* and G (into as Into aFn) x = aFn into x *)
    datatype factFun = Fun of factFun -> int -> int

    val fact =
	let 
	    val fact' =
	     fn (f as Fun f'') =>
		fn 0 => 1
	      | n => n * (f'' f) (n-1)
	in
	    fact' (Fun(fact'))
	end

    val result = fact(5)

    end
