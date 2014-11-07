
signature CONSING_REMEMBERING_ARGUMENTS = 
	sig
		type t
		val cons : t ->	{ 	result : t list,
							arguments_seen_so_far : t list	}
	end

(*
The following signature is more general and
allow to use full type inference:

signature CONSING_REMEMBERING_ARGUMENTS = 
	sig
		val cons : 'a -> 'a ->	{ 	result : 'a list,
                                    arguments_seen_so_far : 'a list	}
	end
*)

(* 
 The following isn't the ortodox way to use type inference
 since we require the client of this functor to specify
 a type instead of giving the fixed element as argument
 of the function. Although it isn't practically useful,
 allow us to explore a different way to use functors,
 in particular define a type to be used in the `declaration'
 headers and type constraints.
 *)
functor ConsingOntoFixElementRememberingArguments (
	type t
	val fixed_element : t)
	:> CONSING_REMEMBERING_ARGUMENTS where type t = t
	=
	struct

	type t = t

	local val arguments_seen_so_far = ref [] in
		fun cons element = 
            let val _ = arguments_seen_so_far := element :: !arguments_seen_so_far 
            in {	result = element :: fixed_element :: [] ,
				    arguments_seen_so_far = !arguments_seen_so_far	} end
	end

	end
