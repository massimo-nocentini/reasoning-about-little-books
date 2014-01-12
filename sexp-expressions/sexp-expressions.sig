signature SEXP = 
sig
    type object

    datatype slist = Null
		   | Cons of sexp * slist
	 and sexp = Atom of object
		  | List of slist
end

functor MakeSexp (structure ObjectType: TYPE)
	:> SEXP where type object = ObjectType.aType
        =
	struct

	type object = ObjectType.aType

	datatype slist = Null
		       | Cons of sexp * slist
	     and sexp = Atom of object
		      | List of slist
	end


