signature SEXP_PARSER_SMLofNJ = SEXP_PARSER 
    where type 'a fragments = 'a SMLofNJ.frag list

functor SExpParserSMLofNJ (structure aSexp: SEXP)
	:> SEXP_PARSER_SMLofNJ 
	=
	struct

    (* maybe this type can be parameterized introducing a functor *)
	type 'a fragments = 'a SMLofNJ.frag list

	exception Parse_error of string

	structure Sexp = aSexp

	local
	    open Sexp
	in
	fun parse (aFragList: 'a fragments): 'a sexp = 
	    let
		open SMLofNJ

		datatype 'a stack_item = OpenParen | Sexp of 'a sexp

		fun % ((QUOTE "(")::rest) aStack = % rest (OpenParen::aStack)
		  | % ((QUOTE ")")::rest) aStack = 
		    let
			fun build_conses (OpenParen::aStackRest) given = 
			    (given, aStackRest)
			  | build_conses ((Sexp aSexp)::aStackRest) given = 
			    build_conses aStackRest (Cons (aSexp, given))
			  | build_conses _ _ = 
			    raise Parse_error "Matching open paren not found in stack in order to build conses."
			val (conses, aStackRest) = build_conses aStack Null
			val inner_list = List conses
		    in
			% rest ((Sexp inner_list)::aStackRest)
		    end
		  | % ((ANTIQUOTE anObject)::rest) aStack = 
		    let 
			val sexp = Atom anObject
		    in
			% rest ((Sexp sexp)::aStack)
		    end
		  | % [] [] = List Null
		  | % [] [Sexp aSexp] = aSexp
		  | % _ _ = raise Parse_error "An invalid sequence of fragments has been discovered."

		(* ``explode_fragment'' has the duty of exploding the
	     caracters ``curried'' by a QUOTE value constructing a new
	     QUOTE value with a string such that contains only one
	     character.*)
		fun explode_fragment (anObject as ANTIQUOTE _) = [anObject]
		  | explode_fragment (QUOTE s) = 
		    let
			fun E ((open_paren as #"(") :: rest) = 
			    [QUOTE (String.str open_paren)] @ (E rest)
			  | E ((close_paren as #")") :: rest) = 
			    [QUOTE (String.str close_paren)] @ (E rest) 
			  | E (_ :: rest ) = E rest
			  | E [] = []
				       
			val chars = String.explode s					   
		    in
			E chars
		    end

		(* ``minify_fraglist'' consumes a list of fragments and
	     foreach fragment attempt to explode the string of
	     character, building a new list of fragments, bigger than
	     the one given as input*)
		fun minify_fraglist [] = []
		  | minify_fraglist (aFragment::rest) = 
		    (explode_fragment aFragment) @ (minify_fraglist rest)

	    in
		% (minify_fraglist aFragList) []
	    end
	end	
end

