structure LittleMLer = 
    struct
    
    (* TODO the following ``open'' should be promoted as parameter of
    a functor, in other word the structure LittleMLer should be
    functorized. *)
    open SExpressions

    (* What is the value of ``prefixer_curried''? It is a function
    that consumes a list and prefixes that list with 1, 2 and 3. *)
    val curried_prefixer = combine_sexp combine_slist_curried 
					(parse `(^(1) ^(2) ^(3))`)

    (* Define a function that is like the value of
    ``prefixer_curried'' *)
    fun prefixer (List aList) = 
	let 
	    val (List (Cons (one, Cons (two, Cons (three, Null))))) = 
		parse `(^(1) ^(2) ^(3))`
	in
	    List (Cons (one, Cons (two, Cons (three, aList))))
	end
      | prefixer (atom as Atom _) = 
	let 
	    val (List (Cons (one, Cons (two, Cons (three, Null))))) = 
		parse `(^(1) ^(2) ^(3))`
	in
	    List (Cons (one, Cons (two, Cons (three, Cons (atom, Null)))))
	end


    (* ``prefixer'' is an approximation of ``prefixer_curried''
     because when ``prefixer'' is used on a list, three Conses happen
     and nothing else. But when the value of ``prefixer_curried'' is
     used, ``combine_sexp'' has only seen the first (Cons 1, _) *)

    (* Aha. Then here is an improvement! Yes, ``waiting_prefixer'' is
     \emph{intensionally} more accurate than ``prefixer''. It does
     mean that the functions ``waiting_prefixer'' and ''prefixer'' are
     \emph{extensionllay} equal because they produce the same values
     when they consume (extensionally) equal values.*)
    fun waiting_prefixer aSexp = 
	let 
	    val rest_of_prefix = parse `(^(2) ^(3))`
	    (* here we have to take apart the result otherwise we've
	     to introduce an additional Cons in order to putting (List
	     combined) onto the result.*)
	    val List combined = combine_sexp combine_slist_curried 
					     rest_of_prefix 
					     aSexp
	in List (Cons (Atom 1, combined)) end

    (* Does \emph{intensionally} mean they differ in how they produce
    the values? Exactly and we can define a function like
    ``combine_sexp'' that produces ``prefixer'' when used with ``parse
    `(^(1) ^(2) ^(3))`'' *)
    val staged_prefixer = 
	let val prefix = parse `(^(1) ^(2) ^(3))`
	in  combine_sexp combine_slist_staged prefix end


    end
