(* Here is a signature for the functor that produces a structure
containing ``similar''. Okay, this is straightforward. A structure
with signature SIMILAR contains two types: number1 and number2. It also
contains a value ``similar'' which consumes two values and produces a
``bool''. *)
signature SIMILAR = 
sig
    type number1
    type number2

    val similar: number1 -> number2 -> bool
end

    (* Does this functor differ from the previous ones? Yes, this one
    depends on two structures, each of which has the signature
    NUMBERS_BY_PEANO. Are the ``where type...'' refinements of SIMILAR
    necessary? Yes, if we ever want to use ``similar''. *)
functor Same (structure a_N: NUMBERS_BY_PEANO
	      structure b_N: NUMBERS_BY_PEANO)
	:> SIMILAR
	       where type number1 = a_N.number
	       where type number2 = b_N.number
	=
	struct
	
	type number1 = a_N.number
	type number2 = b_N.number

	fun sim n m = if a_N.is_zero n 
		      then b_N.is_zero m
		      else sim (a_N.pred n) (b_N.pred m)

	fun similar n m = sim n m
			  handle _ => false

	(* fun similar n m = (sim n m *)
	(* 		  handle a_N.Too_small => false) *)
	(* 		  handle b_N.Too_small => false *)

	end
