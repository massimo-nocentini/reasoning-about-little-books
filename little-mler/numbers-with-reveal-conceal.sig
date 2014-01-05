(* Here is one way out. Let's use a larger signature which requires
that its corresponding structures contain definitions for two
additional functions: ``conceal'' and ``reveal''.  What they can be
about? The function ``conceal'' consumes an ``int'' and produces a
similar ``number''. Does ``reveal'' do the opposite? And the opposite
means that for any ``int'' x: reveal (conceal x) = x. *)
signature NUMBERS_WITH_REVEAL_CONCEAL = 
sig
    include NUMBERS_BY_PEANO
    val conceal: int -> number
    val reveal: number -> int
end

    (* Good here is an extended version of the functor ``NumberAsInt''
    and ``NumberAsNum''. *)
functor NumberAsIntWithRevealConceal ()
	:> NUMBERS_WITH_REVEAL_CONCEAL =
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

	fun conceal n = n
	fun reveal n = n

	end

functor NumberAsNumWithRevealConceal ()
	:> NUMBERS_WITH_REVEAL_CONCEAL =
	struct

	datatype num = Zero | One_more_than of num
						   
	type number = num

	exception Too_small

	fun is_zero Zero = true
	  | is_zero _ = false

	fun succ num = One_more_than num 

	fun pred Zero = raise Too_small
	  | pred (One_more_than n) = n


	fun conceal 0 = Zero
	  | conceal n = One_more_than (conceal (n-1))

	fun reveal Zero = 0
	  | reveal (One_more_than n) = 1 + reveal n

	end




    (* An attempt to factor the code but it is nonsense because the
    definition: *)
(* functor NumberAsIntWithRevealConcealNonsense () *)
(* 	:> NUMBERS_WITH_REVEAL_CONCEAL =  *)
(* 	struct *)

(* 	structure IntStruct = NumberAsInt () *)

(* 	open IntStruct *)

(* 	fun conceal n = n *)
(* 	fun reveal n = n *)

(* 	end *)
    (* produces:
little-mler/numbers-with-reveal-conceal.sig:16.9-27.5 
Error: value type in structure doesn't match signature spec
    name: conceal
  spec:   int -> ?.IntStruct.number
  actual: 'a -> 'a
little-mler/numbers-with-reveal-conceal.sig:16.9-27.5 
Error: value type in structure doesn't match signature spec
    name: reveal
  spec:   ?.IntStruct.number -> int
  actual: 'a -> 'a
 *)
