structure SExpressions = 
struct

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

	(* fun % [(QUOTE ""), (ANTIQUOTE anObject), (QUOTE "")]  *)
	(*       (aStack: 'a stack_item list) = (Atom anObject) before (print "hello") *)
	fun % ((QUOTE "(")::rest) aStack = % rest (OpenParen::aStack)
	  | % ((QUOTE ")")::rest) aStack = 
	    let
		fun build_conses (OpenParen::aStackRest) given = (given, aStackRest)
		  | build_conses ((Sexp aSexp)::aStackRest) given = 
		    build_conses aStackRest (Cons (aSexp, given))
		  | build_conses _ _ = raise Parse_error 
					     "Matching open paren not found in stack in order to build conses."
		val (conses, aStackRest) = build_conses aStack Null
		val inner_list = List conses
	    in
		% rest ((Sexp inner_list)::aStackRest)
	    end
	  (* | % ((QUOTE _)::rest) aStack = % rest aStack *)
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

end
