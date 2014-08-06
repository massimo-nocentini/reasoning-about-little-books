

signature COMBINE_SEXP = 
sig

    type 'a sexp
    val combine: 'a sexp -> 'a sexp -> 'a sexp

end

functor CombineSexpCurried(structure Sexp: SEXP)
	:> COMBINE_SEXP where type 'a sexp = 'a Sexp.sexp 
    = 
	struct

	open Sexp
	
	fun combine (List fst_slist) (List snd_slist) =
	    let 

		(* 
		 The following strategy follow the curry-technique, that is
		 after consuming an argument, it returns a function that consumes
		 the second argument and that returns the combination of the two
		 argument. The important point to observe is that, when only the
		 first argument is supplied, the following function only sees the
		 first Cons (_, _) since, by the curry style, this application
		 produces another function, hence the computation is
		 ``suspended'', not exploring the structure of the first argument
		 beyond the first Cons.
		 *)
		fun C Null another_slist = another_slist
		  | C (Cons (aSexp, slist)) another_slist = 
		    Cons (aSexp, C slist another_slist)

		(*
		 The following version is written observing that in
		 the previous one the argument `another_slist' is getting
		 passed around and around, to be returned finally in the case
		 (C null). However, the previous one shows deeper the currying
		 mechanism since it accepts two arguments.
		 *)
		fun C' Null = sndslist
		  | C' (Cons (aSexp, slist)) = Cons (aSexp, C' slist)

	    in List(C fst_slist snd_slist) end

	end

functor CombineSexpStaged (structure Sexp: SEXP)
	:> COMBINE_SEXP where type 'a sexp = 'a Sexp.sexp 
    = 
	struct

	open Sexp

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
	fun combine (List fst_slist) (List snd_slist) =
		let
			fun combine_slists_staged Null = 
				(fn anotherList => anotherList)
			|	combine_slists_staged (Cons (aSexp, aList)) = 
				make_cons aSexp (combine_slists_staged aList)
			and make_cons aSexp stage = 
				(fn anotherList => Cons (aSexp, stage anotherList))
		in 
			List (combine_slist_staged fst_slist snd_slist)	
		end

	end
