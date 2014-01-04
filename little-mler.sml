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


    (* ******************10. Building on blocks****************** *)
    exception Too_small

    type our_int = int
    local 
	fun plus (n:our_int) (m:our_int) = 
	    if is_zero n 
	    then m else succ (plus (pred n) m)
	and is_zero 0 = true
	  | is_zero _ = false
	and succ n = n + 1
	and pred 0 = raise Too_small
	  | pred n = n - 1
    in
    val int_plus = plus
    end

    (* we put the ``num'' datatype definition outside the ``local...''
     environment in order to write some simple tests in
     ``scratch-input.sml'' file. Usually it should go into the
     enviroment...*)
    datatype num = Zero | One_more_than of num
    local
	fun plus n m = 
	    if is_zero n 
	    then m else succ (plus (pred n) m)
	and is_zero Zero = true
	  | is_zero _ = false
	and succ num = One_more_than num (* here ``num'' could be removed but the compiler complains *)
	and pred Zero = raise Too_small
	  | pred (One_more_than n) = n
    in
    val num_plus = plus
    end

    (* Isn't it curious that the two definitions of ``plus'' are
    identical? Yes, and that's good.  Why? Because the functions are
    closely related. They produce similar values when they consume
    similar pairs of values. But, up to this point, we can't use
    ``plus'' with both ints and nums. This is unfortunate but, because
    the two definitions of ``plus'' are identical, we must use
    ``building blocks'' with the same names, even though they consume
    and produce values of different types.*)

    (* Any ideas about what to do? There seems to be no other way to
    do this. For each definition of ``plus'' we need to have around
    the two sets of building blocks. Each set requires definitions for
    the same set of names. Because it is impossible to use a name for
    two different definitions, we cannot have two definitions of
    ``plus'' available at the same time. *)

    (* There is a way and we are about to discover it. Oh great. What
    are the basic building blocks needed to make ``plus''? There are
    five: the type, the exception ``Too_small'' and the functions
    ``succ, pred, is_zero''. If we call the type ``number'', the types
    of the building blocks are ``number -> number'' and ``number ->
    bool'' respectively.*)

    (* Good, and here is a way to write down these minimal
    requirements for our building blocks *)
    signature NUMBERS_BY_PEANO =
    sig
    	type number
    	exception Too_small
    	val succ: number -> number
    	val pred: number -> number
    	val is_zero: number -> bool
    end

    end
