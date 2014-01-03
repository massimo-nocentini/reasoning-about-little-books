structure LittleMLer = 
    struct
    
    (* TODO the following ``open'' should be promoted as parameter of
    a functor, in other word the structure LittleMLer should be
    functorized. *)
    open SExpressions

    (* What is the value of ``prefixer_curried''? It is a function
    that consumes a list and prefixes that list with 1, 2 and 3. *)
    val prefixer_curried = combine_sexp combine_slist_curried (parse `(^(1) ^(2) ^(3))`)

    (* Define a function that is like the value of
    ``prefixer_curried'' *)
    fun prefixer aSexp = 
	let val prefix = parse `(^(1) ^(2) ^(3))`
	in List (Cons (prefix, Cons (aSexp, Null))) end

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
	let val rest = parse `(^(2) ^(3))`
	in List (Cons (Atom 1, 
		       Cons (combine_sexp combine_slist_curried rest aSexp, 
			     Null))) 
	end

    (* Does \emph{intensionally} mean they differ in how they produce
    the values? Exactly and we can define a function like
    ``combine_sexp'' that produces ``prefixer'' when used with ``parse
    `(^(1) ^(2) ^(3))`'' *)
    val staged_prefixer = 
	let val prefix = parse `(^(1) ^(2) ^(3))`
	in  combine_sexp combine_slist_staged prefix end


    end
