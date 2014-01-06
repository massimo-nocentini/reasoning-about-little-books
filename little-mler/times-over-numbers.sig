signature T = 
sig
    type number
    val times: number -> number -> number
end

    (* Here is the functor...we go out to dinner. *)
functor TimesOverNumber (structure a_N: NUMBERS_BY_PEANO
			 structure a_P: PLUS_OVER_NUMBER
					    sharing type a_N.number = 
							 a_P.number)
	:> T where type number = a_N.number
	=
	struct

	type number = a_N.number

	fun times n m = if a_N.is_zero m
			then m
			else a_P.plus n (times n (a_N.pred m))

	end
