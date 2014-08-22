


signature SEXP_REMBER_ONE_STAR =
	sig
		type 'a sexp
		
		val rember_one_star : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end


functor SexpRemberOneStar (
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp )
	:> SEXP_REMBER_ONE_STAR where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun rember_one_star (List slist) (Atom target) comparer = 
		let
			fun R (empty as Null) = empty
			|	R (Cons ((atom_sexp as Atom atom), cdr_slist)) = 
					if comparer atom target
					then cdr_slist else Cons (atom_sexp, R cdr_slist)
			|	R (Cons ((list_sexp as List slist), cdr_slist)) =
					let val processed = List (R slist) in	
						if SexpEqualFunction.equal comparer list_sexp processed
						then Cons (list_sexp, R cdr_slist) 
						else Cons (processed, cdr_slist) 
					end


		in List (R slist)  end


	end

(*
 Didn't we say that `leftmost' and `rember_one_star' are related?
 Yes, we did. Is `rember_one_star' also a function that finds the
 final result yet checks many times that it did? No, in that regard 
 `rember_one_star' is quite different. Every time it finds that the
 car of a list is a list, it works through the car and checks right
 afterwards with `equal' whether anything changed. Does `rember_one_star'
 know when it failed to accomplish anything? It does: every time it
 encounters the empty list, it failed to find the atom that is supposed
 to be removed. Can we help `rember_one_star' by using a compass needle
 when it finds the empty list? With the help of a North Pole and a 
 compass needle, we could abruptly and promptly signal that the list
 in the car of a list did not contain the interesting atom.
*)
functor SexpRemberOneStarWithLetcc (
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp
	structure HopSkipAndJump : HOP_SKIP_AND_JUMP )
	:> SEXP_REMBER_ONE_STAR where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	(*
	 There is an easier way other than define the following datatype:
	 	USE THE BUILT-IN 'a option type!
	 *)
	datatype 'a target_holder   =   TargetNotPresent 
							    |	SListWithoutTarget of 'a slist


    fun rember_one_star (sexp as List slist) (target_sexp as Atom target) comparer = 
        let

            (*
             The following is a sketch of this idea: It sets up a North Pole and
             then recurs on the car also using the corresponding compass needle.
             When it finds the empty list it uses the compass needle to get back
             to a place where it should explore the cdr of a list.
             *)
            fun rm Null oh = oh TargetNotPresent
            |	rm (Cons ((atom_sexp as Atom atom), cdr_slist)) oh = 
                    if comparer atom target
                    then SListWithoutTarget cdr_slist
                    else let val SListWithoutTarget processed_cdr = rm cdr_slist oh
                            in SListWithoutTarget (Cons (atom_sexp, processed_cdr)) end
            |	rm (Cons (car_sexp as List car_slist, cdr_slist)) oh =
                    (*
                     What kind of value does `letcc...' yield when `car_sexp' does
                     not contains `target_sexp'? The value `TargetNotPresent'.
                     And what kind of value do we get when `car_sexp' contains
                     `target_sexp'? A `SListWithoutTarget' with the first occurrence
                     of `target_sexp' removed. Then what do we need to check next? We 
                     need to ask (by pattern matching) whether or not this value is
                     `TargetNotPresent' or not. And then? If it is, `rm' must try to
                     remove `target_sexp' in `cdr_slist'. Is this the only thing we
                     have to do? No, we must not forget to "cons" the unaltered `car_sexp'
                     when we succeed, creating a new SListWithoutTarget value since `rm' has
                     to return values of `target_holder' type.
                     And if `letcc...''s value is not a `TargetNotPresent'? Then it is a 
                     `SListWithoutTarget' carrying a slist, which means that `rm' succeeded
                     in removing the first occurrence of `target_sexp' from `car_sexp' .
                     How do we build the result in this case? We cons the very value
                     that `letcc...' produced onto `cdr_slist', which does not change.
                     *)
                    case HopSkipAndJump.letcc (fn oh_car => rm car_slist oh_car) 
                    of	TargetNotPresent		            =>
                            let val SListWithoutTarget processed_cdr = rm cdr_slist oh
                            in SListWithoutTarget (Cons (car_sexp, processed_cdr)) end
                    |	SListWithoutTarget processed_car	=> 
                            SListWithoutTarget (Cons (List processed_car, cdr_slist))

        in  
            (*
             How can we use `rm'? We need to set up a North Pole first. Why? If the list
             doesn't contain the atom we want to remove, we must be able to say `TargetNotPresent',
             and if it is the case, we simply return the given sexp, unaltered.
             
             *)
            case HopSkipAndJump.letcc (fn oh => rm slist oh) 
            of  TargetNotPresent                    => sexp
            |   SListWithoutTarget processed_slist  => List processed_slist 
        end


	end

(*
 Try this hot fudge sundae with coffee ice cream for dessert:
 *)
functor SexpRemberOneStarWithTry (
	structure Sexp : SEXP 
	structure SexpEqualFunction : SEXP_EQUAL 
	sharing type Sexp.sexp = SexpEqualFunction.sexp
	structure HopSkipAndJump : HOP_SKIP_AND_JUMP )
	:> SEXP_REMBER_ONE_STAR where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	datatype 'a target_holder   =   TargetNotPresent 
							    |	SListWithoutTarget of 'a slist


    fun rember_one_star (sexp as List slist) (target_sexp as Atom target) comparer = 
        let

            fun rm Null is_target_present = is_target_present TargetNotPresent
            |	rm (Cons ((atom_sexp as Atom atom), cdr_slist)) if_target_is_in_cdr_slist = 
                    if comparer atom target
                    then SListWithoutTarget cdr_slist
                    else let val SListWithoutTarget processed_cdr = rm cdr_slist if_target_is_in_cdr_slist 
                            in SListWithoutTarget (Cons (atom_sexp, processed_cdr)) end
            |	rm (Cons (car_sexp as List car_slist, cdr_slist)) if_target_is_in_cdr_slist =
                    HopSkipAndJump.try 
                        (fn if_target_is_in_car_slist  => 
                            let val SListWithoutTarget processed_car = rm car_slist if_target_is_in_car_slist
                            in SListWithoutTarget (Cons (List processed_car, cdr_slist)) end ) 
                        (fn otherwise => 
                            let val SListWithoutTarget processed_cdr = rm cdr_slist if_target_is_in_cdr_slist
                            in SListWithoutTarget (Cons (car_sexp, processed_cdr)) end )

        in HopSkipAndJump.try 
            (fn if_target_is_in_slist => 
                let val SListWithoutTarget processed_slist = rm slist if_target_is_in_slist
                in List processed_slist end ) 
            (fn otherwise => sexp)
        end


	end

