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
    requirements for our building blocks...open
    ``numbers-by-peano.sig'' to continue the adventure, we'll wait
    here... *)

    (* Have you seen ``NumberAsNum'' and ``NumberAsInt''? Good, let's
    use a functor to build a structure. What is the signature of
    ``IntStruct''? It is ``NUMBERS_BY_PEANO'' obviously, because the
    definition of ``NumberAsInt'' states that the functor produces
    structures with signature ``NUMBERS_BY_PEANO''. And what does
    ``()'' behind ``NumberAsInt'' mean? It means that we are using a
    functor that does not depend on anything else.*)
    structure IntStruct = NumberAsInt()

    (* Define the structure NumStruct *)
    structure NumStruct = NumberAsNum()
				     
    (* Why are we doing all of this? Is it because we want to use both
    versions of ``plus'' at the same time and, if possible, create
    them from the same text? *)

    (* Do we now have both sets of building blocks around at the same
    time? Basically. Those for ``num''s are collected in ``NumStruct''
    and those for ``int''s in ``IntStruct''. Is this progress? Yes, if
    we can now somehow create the two versions of ``plus'' from the
    two structures. Go read ``plus-over-number.sig'' and come back...*)

    (* ...okay, let's build a structure from ``PlusOverNumber'', pay
    attention how to satisfy PlusOverNumber dependency. Yet more
    notation? Yes, consider the functor's dependency: ``(structure a_N
    : NUMBERS_BY_PEANO)'', it specify that the structure created by
    PlusOverNumber depends on a yet to be determined structure ``a_N''
    with signature NUMBERS_BY_PEANO. Here we say that ``a_N'' stands
    for ``IntStruct''. *)
    structure IntArith = PlusOverNumber (structure a_N = IntStruct)

    (* Does ``IntStruct'' have the signature NUMBERS_BY_PEANO? That
    structure was created with ``NumberAsInt'', which always produces
    structures that have signature NUMBERS_BY_PEANO. And how do we
    know that? The definition of ``NumberAsInt'' contains
    NUMBERS_BY_PEANO after :>, and that's what says the resulting
    structure has signature NUMBERS_BY_PEANO. *)

    (* Time to create ``plus'' over ``num''s! *)
    structure NumArith = PlusOverNumber (structure a_N = NumStruct)

    (* What is the value of ``IntArith.plus 3 4''? *)
    (* val nonsense = IntArith.plus 3 4 *)
    (* We've commented the above value definition since the compiler
    complains: operator domain: IntArith.number operand: int in
    expression: IntArith.plus 3*)

    (* What do we know about ``IntArith''? We know that it is a
    structure that has signature PLUS_OVER_NUMBER. What do we know
    about strutures that have signature PLUS_OVER_NUMBER? A structure
    with signature PLUS_OVER_NUMBER has two components: a type named
    ``number'' and a value named ``plus''. The value ``plus'' consumes
    two ``number'' and produces one. And what else do we know about
    the type ``number'' in PLUS_OVER_NUMBER? Nothing because the
    signature PLUS_OVER_NUMBER does not reveal anything else about the
    structures that PlusOverNumber produces. Absolutely. And that's
    why it is nonsense to ask for the value of `` IntArith.plus 3
    4''. Okay, that's clear. The function ``IntArith.plus'' consumes
    values of type ``IntArith.number'', about which PLUS_OVER_NUMBER
    doesn't reveal anything, but 3 and 4 are ``int''s.*)

    (* Can we determine the value of *)
    (* val another_nonsense = NumArith.plus (One_more_than Zero)  *)
    (* 					 (One_more_than (One_more_than Zero)) *)
    (* Nonsense: little-mler/little-mler.sml:187.9-188.43 Error:
  operator and operand don't agree [tycon mismatch] operator domain:
  NumArith.number operand: num in expression: NumArith.plus
  (One_more_than Zero). The function ``NumArith.plus'' consumes values
  of type ``NumArith.number'', but ``One_more_than'' and ``Zero'' are
  ``num''s. *)

    (* Do we have the means to produce ``number''s of the correct type
    for either ``IntArith.plus'' or ``NumArith.plus''? No, the two
    structures contains only one function, ``plus'', and it assumes
    that we have ``number''s ready for consumption. How about the
    structures ``IntStruct'' and ``NumStruct''? They, too, provide
    only functions that consume existing ``number''s. So what do we
    do? Yes, what? Go read ``numbers-with-reveal-conceal.sig'' and
    come back...*)

    (* ...okay, let's rebuild the structures ``IntStruct'' and
    ``NumStruct'' with reveal and conceal... *)
    structure IntStructWithRevealConceal = NumberAsIntWithRevealConceal ()
    structure NumStructWithRevealConceal = NumberAsNumWithRevealConceal ()

    (* ...and their arithmetic respectively *)
    structure IntArithWithRevealConceal = PlusOverNumber (
	structure a_N = IntStructWithRevealConceal)

    structure NumArithWithRevealConceal = PlusOverNumber (
	structure a_N = NumStructWithRevealConceal)

    (* What kind of structures are ``IntStructWithRevealConceal'' and
    ``NumStructWithRevealConceal''? Both have signature
    NUMBERS_WITH_REVEAL_CONCEAL. What kind of structure does
    PlusOverNumber depend on? It depends on a structure with signature
    NUMBERS_BY_PEANO. Isn't this a conflict? Does a structure with
    signature NUMBERS_WITH_REVEAL_CONCEAL provide all the things that
    a structure with signature NUMBERS_BY_PEANO provides? It does,
    because NUMBERS_WITH_REVEAL_CONCEAL ``include''s
    NUMBERS_BY_PEANO. Absolutely and that's why it is okay to supply
    ``IntStructWithRevealConceal'' and ``NumStructWithRevealConceal''
    to functor ``PlusOverNumber''. Okay, take a look at the test
    methods ``reveal_conseal_of_IntStruct'' and
    ``reveal_conseal_of_NumStruct'' to see that they have sense!*)

    end
