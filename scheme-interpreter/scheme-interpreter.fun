
structure SchemeInterpreterEnvironment = 
    struct

    datatype scheme_term = car | cdr

    functor MakeTypeSchemeTerm () 
	    :> TYPE where type aType = scheme_term
            =
	    struct type aType = scheme_term end


    functor SchemeInterpreter () 
	    :> SEXP_INTERPRETER where type term = scheme_term
            =
	    struct

	        type term = scheme_term

		structure Sexp = MakeSexp (
		    structure ObjectType = MakeTypeSchemeTerm ())
			
		(* type action = term Sexp.sexp -> ta *)

		(* val sexp_to_action: term Sexp.sexp -> action *)


	    end


    end
