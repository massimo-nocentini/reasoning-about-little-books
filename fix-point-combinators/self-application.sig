


signature SELF_APPLICATION = 
sig
    datatype 'a T = Into of 'a T -> 'a
end

signature ATTEMPT =
sig

    type 'a T
    type ('a,'b,'c) two_argument

    val application: ('a,'b,'c) two_argument T -> ('a,'b,'c) two_argument

end

functor TwoArgumentSelfApplication (
    structure SelfApplication : SELF_APPLICATION)
	: ATTEMPT where type 'a T = 'a SelfApplication.T
        =
	struct

	type 'a T = 'a SelfApplication.T
	type ('a,'b,'c) two_argument = 'a -> 'b -> 'c

	(* type 'a T = 'a SelfApplication.T *)
	open SelfApplication

	fun application (into as SelfApplication.Into aFn) x1 x2 =
	    aFn into x1 x2

	end

signature SELF_APPLICATION_OPERATORS = 
sig
    type 'a T

    val G0 : (unit -> 'a) T -> 'a
    val G1 : ('a -> 'b) T -> 'a -> 'b
    val G2 : ('a -> 'b -> 'c) T -> 'a -> 'b -> 'c
    val G3 : ('a -> 'b -> 'c -> 'd) T -> 'a -> 'b -> 'c -> 'd
    val G4 : ('a -> 'b -> 'c -> 'd -> 'e) T -> 'a -> 'b -> 'c -> 'd -> 'e
    val G5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) T
             -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val G6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) T
             -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val G7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) T
             -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
    val G8 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) T
             -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i
    val G9 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j) T
             -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j
    val G10 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k) T
              -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k
end
