functor LittleSchemer (structure SexpStr : SEXP) = 
    struct

    structure SexpParser = SExpParserSMLofNJ (
	structure aSexp = SexpStr)

    structure SexpFunctions = SexpFunctionsStandardImpl (
	structure Sexp = SexpStr)

    open SexpStr SexpParser SexpFunctions

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
	      | R_from_slist (Cons (atom as Atom a, alist)) = 
		if pred a
		then R_from_slist alist
		else Cons (atom, R_from_slist alist)
	      | R_from_slist (Cons (List fsList, snList)) = 
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
		fn slist => 
		   case slist of
		       Null => Null
		     | Cons (atom as Atom a, alist) =>
		       if pred a 
		       then curry_maker hukairs alist
		       else Cons (atom, curry_maker hukairs alist)
		     | Cons (List fsList, snList) => 
		       Cons (List (curry_maker hukairs fsList), 
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
		fn slist =>
		   case slist of 
		       Null => Null
		     | Cons (atom as Atom a, alist) =>
		       if pred a 
		       then curry_maker hukairs alist
		       else Cons (atom, curry_maker hukairs alist)
		     | Cons (List fsList, snList) => 
		       Cons (List (curry_maker hukairs fsList), 
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

    (* fun Y f = H f (Into (H f)) *)
    (* (* and H f into = f (G into) *) *)
    (* and H f = f o G *)
    fun G (into as Into aFn) x = aFn into x
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
		fn slist => 
		   case slist of 
		       Null => Null
		     | Cons (atom as Atom a, alist) =>
		       if pred a 
		       then G future alist
		       else Cons (atom, G future alist)
		     | Cons (List fsList, snList) => 
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
		fn slist =>
		   case slist of
		       Null => Null
		     | Cons (atom as Atom a, alist) =>
		       if pred a 
		       then (fn arg => G future arg) alist
		       else Cons (atom, (fn arg => G future arg) alist)
		     | Cons (List fsList, snList) => 
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
		    fn slist =>
		       case slist of 
			   Null => Null
			 | Cons (atom as Atom a, alist) =>
			   if pred a 
			   then recfun alist
			   else Cons (atom, recfun alist)
			 | Cons (List fsList, snList) => 
			   Cons (List (recfun fsList), recfun snList)) 
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
			fn slist =>
			   case slist of 
			       Null => Null
			     | Cons (atom as Atom a, alist) =>
			       if pred a 
			       then recfun alist
			       else Cons (atom, recfun alist)
			     | Cons (List fsList, snList) => 
			       Cons (List (recfun fsList), recfun snList)) 

	    fun function_maker (future as (Into _)) =
		M (fn arg => G future arg)

	    val Mrember_curry = function_maker (Into function_maker)
    	in
	    List (Mrember_curry slist)
    	end
      | remove_from_sexp_abridged_toward_Y_step_6 _ _ =
    	raise RemoveCannotBeAppliedToAtomicSexp

    (* Write ``Mrember_curry'' without using ``function_maker''! Hint:
     use the most recent definition of ''function_maker'' in two
     different places.*)
    fun remove_from_sexp_abridged_toward_Y_step_7 pred (List slist)=
    	let
	    val M = fn recfun => 
		       fn slist =>
			  case slist of
			      Null => Null
			    | Cons (atom as Atom a, alist) =>
			      if pred a 
			      then recfun alist
			      else Cons (atom, recfun alist)
			    | Cons (List fsList, snList) => 
			      Cons (List (recfun fsList), recfun snList)

	    val function_maker = fn (future as (Into _)) =>
				    M (fn arg => G future arg)

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
	    val M = fn recfun => 
		   fn slist =>
		      case slist of 
			  Null => Null
			| Cons (atom as Atom a, alist) =>
			  if pred a 
			  then recfun alist
			  else Cons (atom, recfun alist)
			| Cons (List fsList, snList) => 
			  Cons (List (recfun fsList), recfun snList)

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

    (* You have just worked through the derivation of a function
     called ``the applicative-order Y-combinator''. The interesting
     aspect of Y is that it produces recursive definitions without the
     bother of requiring that the functions be named with ``fun ...''
     *)
    val Y = fn M => 
	       (fn (future as (Into _)) => M (fn arg => G future arg)) 
		   (Into (fn (future as (Into _)) =>
			     M (fn arg => G future arg)))

    (* Define a helper function ``L'' so that ``length'' is ``Y L'' *)
    fun length (List aList) =
	let
	    fun L length Null = 0
	      | L length (Cons (Atom _, cdr)) =
	      	1 + length cdr
	      | L length (Cons (List innerList, cdr)) = 
		(length innerList) + (length cdr)
	in
	    Y L aList
	end
      | length (Atom _) = 1

    (* Describe in your own words what a function ``f'' should be for
    ``Y f'' to work as expected!  Felleisen & Friedman words: ``f'' is
    a function which we want to be recursive, except that the argument
    ``recfun'' replaces the recursive call, and the whole expression
    is wrapped in ``fn recfun => ...'' *)

    (* Write ``length'' using Y, but not L, by substituting the
    definition for L. We do this step because the name ``L'' doesn't
    appear in the body of ``L'', so ``L'' doesn't need to be defined
    with ``fun L ...'' *)
    fun length_without_L (List aList) =
	    Y (fn length => 
		  fn slist =>
		     case slist of 
			 Null => 0
		       | (Cons (Atom _, cdr)) =>
	      		 1 + length cdr
		       | (Cons (List innerList, cdr)) =>
			 (length innerList) + (length cdr)) 
	      aList
      | length_without_L (Atom _) = 1

    (* Does the Y-combinator need to be named with ``fun Y ...''? No,
     because Y doesn't appear in its own definition. So rewrite
     ``length'' without using either Y or L!*)
    fun length_without_L_and_Y (List aList) =
	(fn M => (fn (future as (Into _)) => M (fn arg => G future arg)) 
		     (Into (fn (future as (Into _)) =>
			       M (fn arg => G future arg)))) 
	    (fn length => 
		fn slist => 
		   case slist of 
		       Null => 0
		     | (Cons (Atom _, cdr)) =>
		       1 + length cdr
		     | (Cons (List innerList, cdr)) =>
		       (length innerList) + (length cdr)) 
	    aList
      | length_without_L_and_Y (Atom _) = 1

    (* We observe that ``length'' does not need to be defuned with
     ``fun length ...''.  Write an application that corresponds to
     ``(length aList)'' without using ``length''!*)
    val length_undefuned: int = 
	let 
	    val hello = "hello"
	    val List aList = (parse `(((^hello ()) ^hello (((^hello)) () (^hello)) ()) ((() ^hello)))`)
	in
	    (fn M => (fn (future as (Into _)) => M (fn arg => G future arg)) 
			 (Into (fn (future as (Into _)) =>
				   M (fn arg => G future arg))))
		(fn length => 
		    fn slist =>
		       case slist of
			   Null => 0
			 | (Cons (Atom _, cdr)) =>
			   1 + length cdr
			 | (Cons (List innerList, cdr)) =>
			   (length innerList) + (length cdr))
		aList
	end

    (* Does your hat still fit? Perhaps not, if your mind has been
    stretched. *)

    (* All the previous derivation weren't been possible without the following
     http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html *)

    (* datatype factFun = Fun of factFun -> int -> int *)

    (* val fact = *)
    (* 	let fun fact' (f: factFun) n = *)
    (* 		case f of Fun(f'') => *)
    (* 			  if n = 0 then 1 else n * (f'' f) (n-1) *)
    (* 	in *)
    (* 	    fact' (Fun(fact')) *)
    (* 	end *)

    (* val result = fact(5) *)

    val Y_multiarg = fn G => 
			fn M => 
			   (fn (future as (Into _)) => M (G future)) 
			       (Into (fn (future as (Into _)) =>
					 M (G future)))

    fun G0 (into as Into aFn) = 
	aFn into ()
    fun G1 (into as Into aFn) x1 = 
	aFn into x1
    fun G2 (into as Into aFn) x1 x2 = 
	aFn into x1 x2
    fun G3 (into as Into aFn) x1 x2 x3 = 
	aFn into x1 x2 x3
    fun G4 (into as Into aFn) x1 x2 x3 x4 = 
	aFn into x1 x2 x3 x4
    fun G5 (into as Into aFn) x1 x2 x3 x4 x5 = 
	aFn into x1 x2 x3 x4 x5
    fun G6 (into as Into aFn) x1 x2 x3 x4 x5 x6 =
	aFn into x1 x2 x3 x4 x5 x6 
    fun G7 (into as Into aFn) x1 x2 x3 x4 x5 x6 x7 = 
	aFn into x1 x2 x3 x4 x5 x6 x7
    fun G8 (into as Into aFn) x1 x2 x3 x4 x5 x6 x7 x8 = 
	aFn into x1 x2 x3 x4 x5 x6 x7 x8
    fun G9 (into as Into aFn) x1 x2 x3 x4 x5 x6 x7 x8 x9 = 
	aFn into x1 x2 x3 x4 x5 x6 x7 x8 x9
    fun G10 (into as Into aFn) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = 
	aFn into x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 

    (* val Y = Y_multiarg G1 *)

    (* val Yfour : (('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c ->
    'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e = *)
    (* 	Y_multiarg G4 *)
    (* little-schemer.sml:352.9-353.15 Error: explicit type variable
cannot be genneralized at its binding declaration: 'a *)
    (* little-schemer.sml:352.9-353.15 Error: explicit type variable
cannot be generalized at its binding declaration: 'b *)
    (* little-schemer.sml:352.9-353.15 Error: explicit type variable
cannot be generalized at its binding declaration: 'c *)
    (* little-schemer.sml:352.9-353.15 Error: explicit type variable
cannot be generalized at its binding declaration: 'd *)
    (* little-schemer.sml:352.9-353.15 Error: explicit type variable
cannot be generalized at its binding declaration: 'e *)

    (* Define a helper function ``L'' so that ``length'' is ``Y L'' *)
    fun length_with_accumulator (List aList) =
	let
	    fun L length Null acc = acc
	      | L length (Cons (Atom _, cdr)) acc =
	      	length cdr (1 + acc)
	      | L length (Cons (List innerList, cdr)) acc = 
		let 
		    val acc_of_car_list = (length innerList 0) 
		in 
		    length cdr (acc + acc_of_car_list)
		end
	in
	    Y_multiarg G2 L aList 0
	end
      | length_with_accumulator (Atom _) = 1

    (* Define a helper function ``L'' so that ``length'' is ``Y L'' *)
    fun length_with_collector collector (List aList) =
	let
	    fun L length Null col = col 0
	      | L length (Cons (Atom _, cdr)) col =
	      	length cdr (fn future_value => col (1 + future_value))
	      | L length (Cons (List innerList, cdr)) col = 
		length innerList (
		    fn future_length_of_car_list => 
		       length cdr (fn future_length_of_cdr_list => 
				      col (future_length_of_car_list + 
					   future_length_of_cdr_list)))
	in
	    Y_multiarg G2 L aList collector
	end
      | length_with_collector collector (Atom _) = collector 1

 
    end
