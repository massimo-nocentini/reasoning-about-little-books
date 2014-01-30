functor LittleMLer (structure SexpStr : SEXP) = 
    struct

    structure SexpParser = SExpParserSMLofNJ (
	structure aSexp = SexpStr)

    structure SexpFunctions = SexpFunctionsStandardImpl (
	structure Sexp = SexpStr)

    open SexpStr SexpParser SexpFunctions


    (* What is the value of ``prefixer_curried''? It is a function
    that consumes a list and prefixes that list with 1, 2 and 3. *)
    val curried_prefixer = combine combine_slists_curried 
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
     used, ``combine'' has only seen the first (Cons 1, _) *)

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
	    val List combined = combine combine_slists_curried 
					rest_of_prefix 
					aSexp
	in List (Cons (Atom 1, combined)) end

    (* Does \emph{intensionally} mean they differ in how they produce
    the values? Exactly and we can define a function like
    ``combine'' that produces ``prefixer'' when used with ``parse
    `(^(1) ^(2) ^(3))`'' *)
    val staged_prefixer = 
	let val prefix = parse `(^(1) ^(2) ^(3))`
	in  combine combine_slists_staged prefix end


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

    (* What is the value of *)
    (* val nonsense_again = *)
    (* 	let *)
    (* 	    open NumStructWithRevealConceal *)
    (* 	    open NumArithWithRevealConceal *)
    (* 	in reveal (plus (conceal 2) (conceal 8)) end *)
    (* It is non sense, but this time it is ``signature nonsense''.
  operator domain: NumArithWithRevealConceal.number
  operand:         NumStructWithRevealConceal.number
  in expression:
    plus (conceal 2)
 *)

    (* What do we know about NumArithWithRevealConceal? We know that
    it has signature PLUS_OVER_NUMBER. What do we know about
    structures with signature PLUS_OVER_NUMBER? A structure with
    signature PLUS_OVER_NUMBER has two components: a type named
    ``number'' and a value named ``plus''. The value ``plus'' consumes
    two ``number''s and produces one. And what else do we know about
    ``number'' in PLUS_OVER_NUMBER? Nothing!*)

    (* What do we know about structures with signature
    NUMBERS_WITH_REVEAL_CONCEAL? They contain a type, also named
    ``number'', an exception and five functions over ``int''s and
    ``number''s. The function ``conceal'' creates a ``number'' from an
    ``int'', and ``reveal'' translates the ``number'' back into an
    ``int''. Do we know anything else about ``number'' in
    NUMBERS_WITH_REVEAL_CONCEAL? Nothing, because the signature
    NUMBERS_WITH_REVEAL_CONCEAL does not reveal anything else about
    the structure.*)

    (* So, how could we possibly know from just looking at the
    signatures alone that NumStructWithRevealConceal.conceal produces
    the same kind of ``number''s that NumArithWithRevealConceal.plus
    consumes? From the signatures alone, we cannot know that the two
    kinds of ``number''s are the same. Indeed, we could have used two
    different names for these types, like ``number1'' and
    ``number2''. But why does that matter? Because we must be able to
    determine from the signatures, and from the signatures only, that
    the type of an expression make sense. If we cannot, the expression
    is nonsense. This is analogous to expressions and ypes, except
    that now we relate types and signatures.*)

    (* Are there other forms of signature nonsense? *)
    (* val other_form_of_signature_nonsense =  *)
    (* 	NumStructWithRevealConceal.Zero *)
	    (* produces Error: unbound variable or constructor: Zero
	    in path NumStructWithRevealConceal.Zero, because the
	    signature doesn't say anything about a constructor
	    ``Zero'', so we can't know anything about it either. *)

    (* We need to say that ``PlusOverNumber'' produces structures
    whose type ``number'' is the same as the type ``number'' in
    ``a_N'', the functor's dependency. And how do we do that? We
    connect the signature of the structure produced by
    ``PlusOverNumber'' to the structure on which it depends, changing
    ``:> PLUS_OVER_NUMBER'' in ``:> PLUS_OVER_NUMBER where type number
    = a_N.number''.*)

    (* Is ``PLUS_OVER_NUMBER where type number = a_N.number'' a
    signature?Yes it is a signature and therefore can be used after
    ``:>''. A ``where'' clause refines what a signature stands for. So
    here, the signature is like PLUS_OVER_NUMBER but requires that
    ``number'' in the functor's result must be equal to
    ``a_N.number''. And how do we make sure in ``struct...end'' that
    his is the case? We define the type ``number'' to be the type
    ``number'' of the structure ``a_N'''s type ``number''. Do the two
    similar looking lines always go together? For Felleisen and
    Friedman, the do. One makes promises and the other fulfills the
    promises.*)

    (* The two arithmetics looks the same after the above modification
    hence we can ask some values as done in test methods
    ``first_attempt_of_num_plus_using_functors_with_where_clause_in_signature_result''
    and
    ``first_attempt_of_int_plus_using_functors_with_where_clause_in_signature_result''
    *) 

    (* Then there is a second way out. Out of what? Out of the dilemma
    that we don't have anything that PlusOverNumber's ``plus'' can
    consume. Oh yes. We said that enlarging the signature for the
    basic building blocks was one way out. Go read
    ``numbers-by-peano-second-version.sig'' and come back... *)

    (* ... here is yet another definition of ``IntStruct''. What do we
    know about the signature of this structure? It is like
    NUMBERS_BY_PEANO, but we also know that the ``number''s are
    ``int''s. *)
    structure IntStruct' = NumberAsInt'()

    (* Yes. Now take a look at this definition of ``IntArith' '':*)
    structure IntArith' = PlusOverNumberWithWhereClause (
    	structure a_N = IntStruct')
    (* getting the following type informations from the compiler:
structure IntArith' :
    sig
      type number = number
      val plus : number -> number -> number
    end *)
     (* What do we know about its signature? We know that it's like
     PLUS_OVER_NUMBER and that its ``number''s are a_N's
     ``number''s. And what are a_N's ``number''s? Since a_N is
     IntStruct', we know from its signature that the ``number''s are
     ``int''s. So, if IntArith''s ``number''s are those of IntStruct',
     and if IntStruct' ``number''s are ``int''s, what do we know? We
     know that IntArith''s ``number''s are ``int''s. *)

    (* If we would have written the following instead: *)
    (* structure IntArith' = PlusOverNumber ( *)
    (* 	structure a_N = IntStruct') *)
    (* we get the following compiler type information: *)
    (* structure IntArith' : *)
    (*     sig *)
    (*       type number *)
    (*       val plus : number -> number -> number *)
    (*     end *)
    (* Then the type ``number'' of IntArith' remains ``unrevealed''
    and we have the same problem of how to build values that ``plus''
    can consume. *)

    (* Is ``Zero'' the same as ``0''? No, ``0'' is similar to, but not
    really the same as, ``Zero''. Is ``One_more_than (One_more_than
    Zero)'' similar to ``2''? Yes, it is similar to ``2'', but not
    equal to ``2''. Define the function ``similar''. Should it only
    consume ``num''s and ``int''s? No, it should wokr for any two
    structures that have the signature ``NUMBERS_BY_PEANO''. That is
    more interesting. Go read ``similar.sig'' and come back...*)

    (* ...How can we use ``similar''? Since the function can consume
    ``number''s produced by ``a_N.conceal'' and ``b_N.conceal'',
    respectively, we just feed `similar numbers' produces with the
    proper ``conceal'' functions.*)

    (* So let's create a structure that compares ``num''s and
    ``int''s. *)
    structure SimIntNum = Same (structure a_N = IntStructWithRevealConceal
				structure b_N = NumStructWithRevealConceal)

    (* Is there another way to do it? It's just a guess. Good
    guess. Why? Heere is what Felleisen and Friedman would have said:
    "Because we supply one structure for each of the functor's
    dependencies.". Are functors like functions? Yes but they only
    consume and produce structures, not values (really? take a look at
    SMLofNJ documentation...maybe we can supply some values....*)
    structure SimNumInt = Same (structure a_N = NumStructWithRevealConceal
				structure b_N = IntStructWithRevealConceal)

    (* we can also compare ``num''s to ``num''s: *)
    structure SimNumNum = Same (structure a_N = NumStructWithRevealConceal
				structure b_N = NumStructWithRevealConceal)

    (* About the use of the new form of ``plus'', have a look at
    ``J-new-plus.sig''...after we can use NewPlusOverJ directly as: *)
    structure NewPlusStruct = NewPlusOverJ (
	structure a_N = NumStructWithRevealConceal
	structure a_P = PlusOverNumberWithWhereClause (
	    structure a_N = a_N))
    (* How do we know from the signatures that the type ``number'' in
     NumStructWithRevealConceal is the same as the type ``number'' in
     ``PlusOverNumberWithWhereClause ( structure a_N = a_N)''? The
     functor that creates ``a_P'' is defined to produce structures
     that have signature: ``PLUS_OVER_NUMBER where type number =
     a_N.number''. And what is NumStructWithRevealConceal.number in
     our case? We know that we create ``a_P'' from
     ``PlusOverNumberWithWhereClause'' with
     ``NumStructWithRevealConceal''. And therefore the type ``number''
     in NumStructWithRevealConceal and the type ``number'' in ``a_P''
     are equal. Does that mean the sharing constraint is satisfied?
     Yes. *)

    (* Felleisen and Friedman bet that you never thought there was so
    much to say about ``plus''. Define the functor ``TimesOverNumber''
    which defines the function ``times'', using a signature ``T'', go
    read times-over-numbers.sig and come back... *)
    structure TimesStruct = TimesOverNumber (
	structure a_N = NumStructWithRevealConceal
	structure a_P = PlusOverNumberWithWhereClause (
	    structure a_N = a_N))

    (* Don't forget to leave a tip. *)

    (* THE TENTH MORAL: Real programs consist of many
    components. Specify the dependencies among these components using
    signatures and functors. *)

    end
