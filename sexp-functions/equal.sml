
signature SEXP_EQUAL_ABRIDGED =
    sig
        type 'a sexp

        val equal : 'a sexp -> 'a sexp -> bool
    end

functor SexpEqualWithComparer (
    type t
    val comparer : t -> t -> bool
    structure Sexp : SEXP)
    :> SEXP_EQUAL_ABRIDGED where type 'a sexp = t Sexp.sexp 
    =
    struct
    
    type 'a sexp = t Sexp.sexp
    
    local open Sexp in
        fun equal (Atom fst) (Atom snd) = comparer fst snd
    end

    end


    
(*
 The following is an attempt to include in the generation of 
 an SexpEqual structure the comparer function for atoms, but
 it doesn't type checks.
 *)
(*************************************************************
signature EQ_KEY = 
	sig
		type key
		type other

		val equal : key -> other -> bool
	end

functor KeyEqualityFnct (
	type key
	type other
	val equal : key -> other -> bool)
	:> EQ_KEY 	where type key = key
				where type other = other
	=
	struct
		type key 	= key
		type other 	= other
		val equal 	= equal
	end

structure KeyEqualityComparer = KeyEqualityFnct(
	type key 	= int
	type other	= int
	fun equal f s = f = s)


signature SEXP_EQUAL_2 = 
	sig
		structure KeyEqualityComparer : EQ_KEY
		type 'a sexp
		val equal: KeyEqualityComparer.key sexp -> KeyEqualityComparer.other sexp -> bool
	end

functor SexpTypeSpecializer (
	structure Sexp : SEXP
	type t)
	:> SEXP where type 'a sexp = t Sexp.sexp
	=
	struct
		(* 
		 very ugly to duplicate here the definitions
		 contained already in the signature.
		 *)
		datatype 	'a slist = 	Null
		| 						Cons of t sexp * t slist
		and 		'b sexp = 	Atom of t 
		|						List of t slist
	end

functor SexpEqualAbridged(
	structure KeyEqualityComparer : EQ_KEY
	type 'a sexp
	structure Sexp : SEXP where type 'a sexp = KeyEqualityComparer.key sexp)
	:> SEXP_EQUAL_2 where type 'a sexp = 'a Sexp.sexp
	=
	struct
		open Sexp
		structure KeyEqualityComparer = KeyEqualityComparer
		fun equal (Atom key) (Atom other) = KeyEqualityComparer.equal key other
	end
************************************************************)

signature SEXP_EQUAL = 
	sig
		type 'a sexp
		val equal: ('a -> 'b -> bool) -> 'a sexp -> 'b sexp -> bool
	end

functor SexpEqual (
	structure Sexp: SEXP) 
	:> SEXP_EQUAL where type 'a sexp = 'a Sexp.sexp
	=
	struct

	open Sexp

	fun equal eq_fn fst_sexp snd_sexp = 
	    let
			fun equal_sexp (Atom a) (Atom b) = eq_fn a b
			|	equal_sexp (List fst_list) (List snd_list) = 
					equal_slist fst_list snd_list
			|	equal_sexp _ _ = false
			and equal_slist Null Null = true
			|	equal_slist (Cons (fst_cons_sexp, fst_cons_slist))
							(Cons (snd_cons_sexp, snd_cons_slist)) = 
					equal_sexp fst_cons_sexp snd_cons_sexp andalso
					equal_slist fst_cons_slist snd_cons_slist
			|	equal_slist _ _ = false
	    in equal_sexp fst_sexp snd_sexp end

	end
