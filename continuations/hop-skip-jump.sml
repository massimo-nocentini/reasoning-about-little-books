

signature HOP_SKIP_AND_JUMP =
	sig
		structure Cont : CONT
		val letcc_general : (unit -> unit) option -> (('a -> 'b) -> 'a) -> 'a
		val letcc : (('a -> 'b) -> 'a) -> 'a
        val try : (('a -> 'b) -> 'c) -> (unit -> 'c) -> 'c
	end


functor HopSkipAndJumpFunctor (
	structure Cont : CONT)
	:> HOP_SKIP_AND_JUMP
	=

	struct

	structure Cont = Cont

	(* 
	 The following definition shows that calling Cont.throw inhibits
	 every following statements: in the following let where the throw
	 happens we see that the after_throwing thunk is NEVER called at
	 all.
	 *)
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
			Cont.callcc (fn current_continuation => 
				f (fn v => let val () = before_throwing () in Cont.throw current_continuation v end))
	|	letcc_general NONE f = 
			Cont.callcc (fn current_continuation => f (fn v => Cont.throw current_continuation v))


	fun letcc f = letcc_general NONE f

    fun try trying default =    letcc (fn success => 
                                    let val _ = letcc (fn escape => success (trying escape))
                                    in default () end)

	end

structure HopSkipAndJump = HopSkipAndJumpFunctor (
	structure Cont = SMLofNJ.Cont)
