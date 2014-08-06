signature SEXP = 
sig
    (* essentially here we're providing pattern matching capabilities
     as basic building blocks for these types.*)
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
