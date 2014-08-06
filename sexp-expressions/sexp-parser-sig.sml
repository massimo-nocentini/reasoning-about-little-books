
(*
 The following signature is weel defined since define one 
 operation and a relative exception that may occur during
 the parsing. This can be enhanced by supplying an handler 
 function, in order to be pure respect invocation and toward
 call/cc.  
*)
signature SEXP_PARSER = 
sig
    type 'a fragments
    type 'a sexp

    exception Parse_error of string

    val parse: 'a fragments -> 'a sexp
end

