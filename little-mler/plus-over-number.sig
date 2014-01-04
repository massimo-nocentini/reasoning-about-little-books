(* Exactly. What is the type of ``plus''? If ``number'' is the type,
    then ``number -> number -> number'' is the type of
    ``plus''. Define a signature that says that. *)
signature PLUS_OVER_NUMBER = 
sig
    type number
    val plus: number -> number -> number
end

    (* Here is the functor: how does it differ from the functors we
     have seen so far? The names of other functors are always followed
     by ``()''. This one, however, contains something else:
     ``structure a_N : NUMBERS_BY_PEANO''. What does it mean?*)
functor PlusOverNumber (structure a_N : NUMBERS_BY_PEANO)
	:> PLUS_OVER_NUMBER = 
	struct
	
	type number = a_N.number

	fun plus n m = 
	    if a_N.is_zero n
	    then m 
	    else a_N.succ (plus (a_N.pred n) m)

	end

	    (* The notation ``structure a_N : NUMBERS_BY_PEANO'' says
	    that the structure produced by ``PlusOverNumber'' depends
	    on a structure ``a_N'' that has signature
	    ``NUMBERS_BY_PEANO''. And that's how we know that ``a_N''
	    contains a type, an exception and three values: ``is_zero,
	    pred, succ''. What does ``a_N.is_zero'' mean? It means
	    that we are using the value named ``is_zero'' from the
	    structure ``a_N''. And how do we know that ``a_N''
	    contains all these things, in particular ``is_zero''?
	    Because it has signature ``NUMBERS_BY_PEANO''.*)
