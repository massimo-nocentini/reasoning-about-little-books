
structure SchemeInterpreterEnvironment = 
    struct

    datatype scheme_term = car | cdr				     

    functor MakeTypeSchemeTerm () 
	    :> TYPE where type aType = scheme_term
            =
	    struct type aType = scheme_term end

    structure Sexp = MakeSexp (
	structure ObjectType = MakeTypeSchemeTerm ())

    datatype scheme_meaning = Primitive 
			    | Quotation of scheme_term Sexp.sexp
			    | Stub

    functor MakeTypeSchemeMeaning () 
	    :> TYPE where type aType = scheme_meaning
            =
	    struct type aType = scheme_meaning end

    functor MakeInterpreter (
	structure Sexp: SEXP where type object = scheme_term
	structure Table: TABLE) 
	    :> SEXP_INTERPRETER where type term = scheme_term
	                        where type meaning = scheme_meaning
            =
	    struct

	        type term = scheme_term

		structure Sexp = Sexp
			
		type meaning = scheme_meaning

		type action = term Sexp.sexp 
			      -> Table.table 
			      -> meaning

		(* val sexp_to_action: term Sexp.sexp -> action *)

		fun value aSexp = Stub

	    end

    structure Interpreter = MakeInterpreter (
	structure Sexp = Sexp
	structure Table = MakeTableDoubleListImpl (
	    structure IdentifierType = MakeTypeString ()
	    structure StuffType = MakeTypeSchemeMeaning ()))

    end
