signature SEXP_INTERPRETER = 
sig 

    type term

    type meaning

    type identifier

    structure Sexp: SEXP where type object = term
		
    val value: Sexp.sexp -> meaning

    exception EmptyListNotAllowedForNonPrimitiveExpression

    exception IdentifierNotBound of identifier

end
