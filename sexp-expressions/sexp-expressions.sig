signature SEXP = 
sig
    type object

    datatype 'a slist = Null
		      | Cons of (object sexp) * ('a slist)
	 and 'b sexp = Atom of 'b
		     | List of object slist
end

functor MakeSexp (structure ObjectType: TYPE)
	:> SEXP where type object = ObjectType.aType
        =
	struct

	type object = ObjectType.aType

	datatype 'a slist = Null
			  | Cons of (object sexp) * ('a slist)
	     and 'b sexp = Atom of 'b
			 | List of object slist
	end


