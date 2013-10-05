structure HopSkipAndJump =
struct

structure Cont = SMLofNJ.Cont

fun letcc f = Cont.callcc 
		  (fn current_continuation => 
		      f (fn v => Cont.throw current_continuation v))

fun rember_up_to_last a lat =
    letcc (fn skip => 		(* as Alonzo Church would have written *)
	      let
		  fun R (empty as []) = empty
		    | R (car::rest) = 
		      if a = car
		      then skip (R rest)
		      else car :: (R rest)
	      in 
		  R lat
	      end
	  )
end
