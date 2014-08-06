signature SEXP_FUNCTIONS = 
sig
    type 'a sexp
    type 'a slist

    val equal: ('a -> 'b -> bool) -> 'a sexp -> 'b sexp -> bool
    val to_string: ('a -> string) -> 'a sexp -> string
    val is_atom: 'a sexp -> bool
    val combine: ('a slist -> 'a slist -> 'a slist) -> 
		 'a sexp -> 'a sexp -> 'a sexp

    val combine_slists_staged: 'a slist -> 'a slist -> 'a slist
    val combine_slists_curried: 'a slist -> 'a slist -> 'a slist

    val pick: int -> 'a sexp -> 'a sexp
end

signature COMBINE_SEXP = 
sig

    type 'a sexp

    val combine: 'a sexp -> 'a sexp -> 'a sexp

    (* val combine_slists_staged: 'a slist -> 'a slist -> 'a slist *)
    (* val combine_slists_curried: 'a slist -> 'a slist -> 'a slist *)
							   

end

functor CombineSexpCurried(structure aSexp: SEXP)
	:> COMBINE_SEXP where type 'a sexp = 'a aSexp.sexp 
        = 
	struct

	(* type 'a sexp = 'a aSexp.sexp  *)
	open aSexp
	
	(* This version of combine_sexp is a little more general
    respect the one presented in ``The Little MLer'' it consumes a
    strategy for combining two slist as first argument and make some
    adjustment the following two arguments of type 'a sexp. It
    eventually call the given strategy in order to build the final
    List sexp. *)
	fun combine (List fst_slist) (List snd_slist) =
	    let 

		(* The following strategy follow the curry-technique, that is
     after consuming an argument, it returns a function that consumes
     the second argument and that returns the combination of the two
     argument. The important point to observe is that, when only the
     first argument is supplied, the following function only sees the
     first Cons (_, _) since, by the curry style, this application
     produces another function, hence the computation is
     ``suspended'', not exploring the structure of the first argument
     beyond the first Cons.*)
		fun C Null another_slist = another_slist
		  | C (Cons (aSexp, slist)) another_slist = 
		    Cons (aSexp, C slist another_slist)

	    in List(C fst_slist snd_slist) end

	end
