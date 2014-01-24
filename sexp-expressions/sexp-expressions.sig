signature SEXP = 
sig
    datatype 'a slist = Null
		      | Cons of 'a sexp * 'a slist
	 and 'b sexp = Atom of 'b
		     | List of 'b slist
end

functor MakeSexp () 
	:> SEXP
	=
	struct

	datatype 'a slist = Null
			  | Cons of 'a sexp * 'a slist
	     and 'b sexp = Atom of 'b
			 | List of 'b slist

	end

signature SEXP_PARSER = 
sig
    type 'a fragments
    type 'a sexp

    exception Parse_error of string

    val parse: 'a fragments -> 'a sexp
end

