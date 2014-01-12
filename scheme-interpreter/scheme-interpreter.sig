signature SEXP_INTERPRETER = 
sig 

    type term

    type meaning

    structure Sexp: SEXP where type object = term
		
    val value: term Sexp.sexp -> meaning

end
