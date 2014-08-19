signature SEXP = 
sig
    (* essentially here we're providing pattern matching capabilities
     as basic building blocks for these types.*)
    datatype 	'a slist = 	Null
  	| 						Cons of 'a sexp * 'a slist
	and 		'b sexp = 	Atom of 'b
	|						List of 'b slist

	(* 
	 Just a remainder to consider later, we choose to not
	 put here function `to_string' in order to have it as the same
	 level of other functions, it is by no means more special than 
	 others in this way. The only drawback is that we have to put its 
	 definition in file in this current subtree and not in the 
	 functions' subtree.
	*)	

end

signature SEXP_TYPED =
	sig
		include SEXP	
		type elem
		type sexp_typed = elem sexp
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

functor MakeSexpTyped(type t) 
	=
	struct 


	datatype  slist = Null
			  | Cons of  sexp *  slist
	     and  sexp = Atom of t 
			 | List of  slist


	end
(*
functor MakeSexpTypeCompatible(
	type t
	structure Sexp : SEXP)
	:> SEXP where type 'a sexp = t Sexp.sexp
	=
	struct

		open Sexp
		type 'a sexp = t Sexp.sexp

	end
*)
