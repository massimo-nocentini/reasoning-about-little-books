
signature CONSING_REMEMBERING_ARGUMENTS = 
	sig
		type t
		val cons : t ->	{ 	result : t list,
							arguments_seen_so_far : t list	}
	end

functor ConsingOntoFixElementRememberingArguments (
	type t
	val fixed_element : t)
	:> CONSING_REMEMBERING_ARGUMENTS where type t = t
	=
	struct

	type t = t

	local val arguments_seen_so_far = ref [] in
		fun cons element = 
			(arguments_seen_so_far := element :: !arguments_seen_so_far;
			{	result = element :: fixed_element :: [] ,
				arguments_seen_so_far = !arguments_seen_so_far	})
	end

	end
