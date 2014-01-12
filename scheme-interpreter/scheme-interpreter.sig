signature SEXP_INTERPRETER = 
sig 

    type term

    structure Sexp: SEXP where type object = term
			
    (* type action *)

    (* val sexp_to_action: term Sexp.sexp -> action *)



end
