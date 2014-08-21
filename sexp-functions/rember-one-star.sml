


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

functor SexpRemberOneStarWithLetcc (
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

            fun rm Null oh = oh TargetNotPresent
            |	rm (Cons ((atom_sexp as Atom atom), cdr_slist)) oh = 
                    if comparer atom target
                    then SListWithoutTarget cdr_slist
                    else let val SListWithoutTarget processed_cdr = rm cdr_slist oh
                            in SListWithoutTarget (Cons (atom_sexp, processed_cdr)) end
            |	rm (Cons (car_sexp as List car_slist, cdr_slist)) oh =
                    case HopSkipAndJump.letcc (fn oh_car => rm car_slist oh_car) 
                    of	TargetNotPresent		            =>
                            let val SListWithoutTarget processed_cdr = rm cdr_slist oh
                            in SListWithoutTarget (Cons (car_sexp, processed_cdr)) end
                    |	SListWithoutTarget processed_car	=> 
                            SListWithoutTarget (Cons (List processed_car, cdr_slist))

        in  case HopSkipAndJump.letcc (fn oh => rm slist oh) 
            of  TargetNotPresent                    => sexp
            |   SListWithoutTarget processed_slist  => List processed_slist 
        end


	end


