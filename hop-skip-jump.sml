structure HopSkipAndJump =
struct

structure Cont = SMLofNJ.Cont

(* ********************************************************** *)
(* The following definition shows that calling Cont.throw inhibits
every following statements: in the following let where the throw
happens we see that the after_throwing thunk is NEVER called at
all. *)
(* ********************************************************** *)
(* fun letcc_general before_throwing after_throwing f =  *)
(*     Cont.callcc  *)
(* 	(fn current_continuation =>  *)
(* 	    f (fn v =>  *)
(* 		  let *)
(* 		      val () = before_throwing () *)
(* 		      val res = Cont.throw current_continuation v *)
(* 		      val () = after_throwing () *)
(* 		  in *)
(* 		      res *)
(* 		  end *)
(* 	      ) *)
(* 	) *)
(*   | letcc_general  =  *)

(* ********************************************************** *)
(* The following definition shows that calling Cont.throw inhibits
every following statements: in the following let, where the throw
happens, we see that the print invocation NEVER happen at all. *)
(* ********************************************************** *)	     
(* fun letcc_general (SOME before_throwing) f =  *)
(*     Cont.callcc  *)
(* 	(fn current_continuation =>  *)
(* 	    f (fn v =>  *)
(* 		  let *)
(* 		      val () = before_throwing () *)
(* 		      val res = Cont.throw current_continuation v *)
(* 		  in *)
(* 		      (print "."; *)
(* 		      res) *)
(* 		  end *)
(* 	      ) *)
(* 	) *)
(*   | letcc_general NONE f =  *)
(*     Cont.callcc  *)
(* 	(fn current_continuation =>  *)
(* 	    f (fn v => Cont.throw current_continuation v)) *)

fun letcc_general (SOME before_throwing) f = 
    Cont.callcc 
	(fn current_continuation => 
	    f (fn v => 
		  let 
		      val () = before_throwing ()
		  in 
		      Cont.throw current_continuation v
		  end
	      )
	)
  | letcc_general NONE f = 
    Cont.callcc 
	(fn current_continuation => 
	    f (fn v => Cont.throw current_continuation v))


val letcc = letcc_general NONE 

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

fun rember_up_to_last_debugging a lat =
    let
	val counter = ref 0
	val before_thunk = fn () => print ("before_thunk: " ^ 
					   (Int.toString (!counter)))
    in
	(* as Alonzo Church would  have written  *)
	letcc_general (SOME before_thunk)
		      (fn skip => 		
			  let
			      fun R (empty as []) = empty
				| R (car::rest) = 
				  if a = car
				  then 
				      let
					  val _ = counter := !counter + 1
					  val res = skip (R rest)
					  val _ = counter := !counter - 1
				      in
					  (print (Int.toString (!counter));
					   res)
				      end
				      (* ( *)
				      (* counter := !counter + 1; *)
				      (* skip (R rest)) *)
				  else car :: (R rest)
			  in 
			      R lat
			  end
		      )
    end
end
