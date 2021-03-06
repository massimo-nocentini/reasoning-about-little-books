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

signature SEXP_BEWARE_SHADOWS =
    sig
        type 'a sexp
        type 'a slist

        val Cons : 'a sexp -> 'a slist -> 'a slist
        val null : 'a slist
        val Kons : ('a -> int) -> 'a slist
    end

functor SexpBewareShadows () 
    : SEXP_BEWARE_SHADOWS
    =
    struct
    
    type 'a sexp = int
    datatype 'a slist =     Kons of 'a -> int
                        |   null
    
    fun Cons sexp slist = Kons (fn x => 3)        
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
        type 	'a slist = t Sexp.slist
        and 		'b sexp = 	t Sexp.sexp
	end*)

signature SEXP_CONS_CTOR = 
    sig
        type 'a sexp
        type 'a slist
        
        val cons: 'a sexp -> 'a slist -> 'a slist
    end

functor SexpConsCtor (
    structure Sexp : SEXP) 
    :> SEXP_CONS_CTOR   where type 'a sexp = 'a Sexp.sexp
                        where type 'a slist = 'a Sexp.slist
    =
    struct

    type 'a sexp = 'a Sexp.sexp
    type 'a slist = 'a Sexp.slist
    
    fun cons sexp slist = Sexp.Cons (sexp, slist)

    end


