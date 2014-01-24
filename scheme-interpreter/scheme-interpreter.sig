signature SEXP_INTERPRETER = 
sig 

    type term

    type meaning

    type identifier

    (* this structure is necessary only to define the type for sexp
    and allow us to build values that belong to that type *)
    (* structure Sexp: SEXP *)
    type 'a sexp
		
    val value: term sexp -> meaning

    exception EmptyListNotAllowedForNonPrimitiveExpression

    exception IdentifierNotBound of identifier

end
