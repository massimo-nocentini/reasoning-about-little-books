signature SEXP_INTERPRETER = 
sig 

    type term

    type meaning

    structure Sexp: SEXP where type object = term
		
    val value: Sexp.sexp -> meaning

    exception EmptyListNotAllowedForNonPrimitiveExpression

end
