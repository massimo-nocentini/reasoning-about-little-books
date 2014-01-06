(* the word ``signature'' makes a name stand for a signature. Our
 example defines the name ``NUMBERS_BY_PEANO''. The signature is the
 collection of things listed between ``sig'' and ``end''.*)
signature NUMBERS_BY_PEANO =
sig
    (* the word ``type'' indicates that ``number'' is used as the name
    of a type. Do we know anything else about the type? Not much. All
    we know here is that the type is used to describe what the values
    ``succ, pred, is_zero'' consume and produce.*)
    type number

    exception Too_small

    (* the keyword ``val'' meanssays that we must have values of a
     certain kind. In our example all three values are functions over
     ``number''s.*)
    val succ: number -> number
    val pred: number -> number
    val is_zero: number -> bool
end

    (* what is a signature? We have seen a signature, but we don't yet
    know what it is. *)

    (* A signature is like a type ``int -> int''. Each element of this
     type must be a function, and furthermore, each element must
     consume and produce an ``int''. Well, we know what the elements
     of a type like ``int -> int'' are, but what are the elements of a
     signature?*)

    (* The elements are called ``structures'' but we don't usually
    call them elements. We say that a structure has a signature. What
    does a signature say about a structure? A signature describes the
    components of structures. Before we can say that a structure has
    some signature, we must check that it provides all the required
    pieces. *)

    (* Fair enough. Before we say that some function ``f'' has type
    ``int -> int'' we check that it consumes and produces
    ``int''s. But have we seen structures yet? *)

(* Not yet. We produce structures with ``functor'', ``(,)'' and ``
struct...end''. Here is one for ``num''s... *)
functor NumberAsNum () 
	:> NUMBERS_BY_PEANO = 
	struct

	datatype num = Zero | One_more_than of num
						   
	type number = num

	exception Too_small

	fun is_zero Zero = true
	  | is_zero _ = false

	fun succ num = One_more_than num 

	fun pred Zero = raise Too_small
	  | pred (One_more_than n) = n

	end

(* ...here is one for ``int''s. The structure for ``int''s must also
contain the required basic building blocks. *)
functor NumberAsInt () 
	:> NUMBERS_BY_PEANO = 
	struct
	
	type number = int

	exception Too_small

	fun plus (n:number) (m:number) = 
	    if is_zero n 
	    then m else succ (plus (pred n) m)
	and is_zero 0 = true
	  | is_zero _ = false
	and succ n = n + 1
	and pred 0 = raise Too_small
	  | pred n = n - 1

	end

	    (* The word ``functor'' makes a name stand for something
	    that produces structures. We refer to this thing as
	    ``functor''. The first example introduces ``NumberAsNum''
	    as a functor's name, the second one ``NumberAsInt''. Using
	    the functor produces a structure that consists of the
	    collection of definitions enclosed in ``struct...end''. *)

	    (* What does ``()'' mean? It means that we are defining a
	    functor that does not depend on anything else. We'll see
	    things other than (). Okay, and then the meaning of
	    ``depend'' should become clearer. *)

	    (* So what is the notation ``... :> NUMBERS_BY_PEANO''
	    about? It states that the result of using the functor is a
	    structure with signature ``NUMBERS_BY_PEANO''. *)
