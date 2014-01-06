functor NumberAsInt' ()
	:> NUMBERS_BY_PEANO 
	       where type number = int
        =
	struct
	
	type number = int

	exception Too_small

	fun plus (n:number) (m:number) = 
	    if is_zero n 
	    then m else succ (plus (pred n) m)
	and is_zero 0 = true
	  | is_zero _ = false
	and succ n = n + 1
	and pred 0 = raise Too_small
	  | pred n = n - 1

	end

(* The following new functor is nonsense: *)
(* functor NumberAsNum' ()  *)
(* 	:> NUMBERS_BY_PEANO *)
(* 	       where type number = num *)
(*         =  *)
(* 	struct *)

(* 	datatype num = Zero | One_more_than of num *)
						   
(* 	type number = num *)

(* 	exception Too_small *)

(* 	fun is_zero Zero = true *)
(* 	  | is_zero _ = false *)

(* 	fun succ num = One_more_than num  *)

(* 	fun pred Zero = raise Too_small *)
(* 	  | pred (One_more_than n) = n *)

(* 	end *)
	    (* The compiler complains with: Error: unbound type
	    constructor: num *)
	    (* remember everything in ``struct...end'' is invisible,
	    and it is the signature that makes it public. So if
	    ``num'' in ``where type number = num'' does not refer to
	    the datatype definition into ``struct...end'', to what
	    does it refer then? Using the compilation manager, we can
	    assume that ``num'' doesn't refer to any datatype, for
	    this the definition is nonsense. *)
