structure SExpressions = struct

    type 'a frag = 'a SMLofNJ.frag	(* maybe this type can be parameterized introducing a functor *)

    exception Parse_error of string

    datatype 'a slist = Null
		      | Cons of ('a sexp) * ('a slist)
	 and 'b sexp = Atom of 'b
		     | List of 'b slist
				  
    fun parse (aFragList: 'a frag list): 'a sexp = 
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

	    fun explode_fragment (anObject as ANTIQUOTE _) = [anObject]
	      | explode_fragment (QUOTE s) = 
		let
		    fun E ((open_paren as #"(") :: rest) = [QUOTE (String.str open_paren)] @ (E rest)
		      | E ((close_paren as #")") :: rest) = [QUOTE (String.str close_paren)] @ (E rest) 
		      | E (_ :: rest ) = E rest
		      | E [] = []
				   
		    val chars = String.explode s					   
		in
		    E chars
		end

	    fun minify_fraglist [] = []
	      | minify_fraglist (aFragment::rest) = 
		(explode_fragment aFragment) @ (minify_fraglist rest)

	in
	    % (minify_fraglist aFragList) []
	end

    fun wrap_parsing_with f = f o parse

    fun to_string (toStringFun: 'a -> string) (aSexp: 'a sexp) : string =
	let 
	    fun sexp_to_string (Atom anAtom) = toStringFun anAtom
	      | sexp_to_string (List aList) = "(" ^ (slist_to_string aList) ^ ")"
	    and slist_to_string Null = ""
	      | slist_to_string (Cons (aSexp, Null)) = sexp_to_string aSexp
	      | slist_to_string (Cons (aSexp, aList)) = 
		(sexp_to_string aSexp) ^ " " ^ (slist_to_string aList)
	in
	    sexp_to_string aSexp
	end

    fun is_atom (Atom _) = true
      | is_atom _ = false

    (* This version of combine_sexp is a little more general respect
    the one presented in ``The Little MLer'' it consumes a strategy
    for combining two slist as first argument and make some adjustment
    the following two arguments of type 'a sexp. It eventually call
    the given strategy in order to build the final List sexp. *)
    fun combine_sexp combine_slist =
	let 
	    fun put_atom_into_empty_list atom = 
		(List (Cons (atom, Null)))

	    fun C aSexp (atom as Atom _) = 
		(* here we've to invoke again providing as a second
	 argument a newly built list containing the single atom, in
	 order to apply the rule (especially for the case where the
	 first argument is an empty list) of ``combine'' to make the
	 expected result*)
		C aSexp (put_atom_into_empty_list atom)
	      | C (atom as Atom _) aSexp = 
		C (put_atom_into_empty_list atom) aSexp
	      | C (List fst_slist) (List snd_slist) =
		(* since the second sexp is a list, we've to build a list
	 again where to ``splash'' the element in the first list *)
		List (combine_slist fst_slist snd_slist)
	in C end

    (* The following strategy follow the curry-technique, that is
     after consuming an argument, it returns a function that consumes
     the second argument and that returns the combination of the two
     argument. The important point to observe is that, when only the
     first argument is supplied, the following function only sees the
     first Cons (_, _) since, by the curry style, this application
     produces another function, hence the computation is
     ``suspended'', not exploring the structure of the first argument
     beyond the first Cons.*)
    fun combine_slist_curried Null another_slist = another_slist
      | combine_slist_curried (Cons (aSexp, slist)) another_slist = 
	Cons (aSexp, combine_slist_curried slist another_slist)

    (* The following strategy follow the stage-technique, that is
    instead of consuming two arguments in curry-style, it consumes
    only one, allowing to recur on that argument and thus
    ``exploring'' its structure via pattern-matching. Only when the
    sexps in the first argument (and only those at the very first
    level) have been explored, an identity function is produced, in
    order to return the second argument as it is. On the other hand,
    for each Cons encountered, we introduce a function ``make_cons''
    which capture the action of consing each sexp, allowing to recur
    on the first argument since the second argument of ``make_cons''
    has to be evaluated before applying ``make_cons'' body.*)
    fun combine_slist_staged Null = (fn anotherList => anotherList)
      | combine_slist_staged (Cons (aSexp, aList)) = 
	make_cons aSexp (combine_slist_staged aList)
    and make_cons aSexp stage = 
	(fn anotherList => Cons (aSexp, stage anotherList))
	
end
