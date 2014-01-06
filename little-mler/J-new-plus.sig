(* Here is a signature. *)
signature J = 
sig
    val plus: int -> int -> int
end

    (* And here is the functor NewPlusOverJ. Why is this definition
    nonsense? It looks okay. It seems to consume a structure that has
    signature NUMBERS_WITH_REVEAL_CONCEAL and another one with
    signature PLUS_OVER_NUMBER. The structure that it produces seems
    to have signature J. *)
(* 
functor NewPlusOverJ (structure a_N: NUMBERS_WITH_REVEAL_CONCEAL
		      structure a_P: PLUS_OVER_NUMBER)
	:> J
	=
	struct

	fun plus n m = a_N.reveal (a_P.plus (a_N.conceal n)
					    (a_N.conceal m))

	end
*)
	    (* Still it is nonsense, as the compiler complains:
 Error: operator and operand don't agree [tycon mismatch]
  operator domain: ?.number
  operand:         ?.number
  in expression:
    a_P.plus (a_N.conceal n)
 *)
(* By absurd, if it has some sense, we would be able to write *)
(* structure NP1 = NewPlusOverJ ( *)
(*     structure a_N = NumberAsNumWithRevealConceal () *)
(*     structure a_P = PlusOverNumberWithWhereClause ( *)
(* 	NumberAsIntWithRevealConceal ())) *)
(* that is, we could mix a set of building blocks and use it with a
wrong arithmetic: the function a_N.conceal would produce numbers as
``num''s and ``a_P.plus'' would attempt to consume those, which is
nonsense. Why it is nonsense? Because ``a_P.plus'' consumes
``int''s.*)

(* So what should we do? We must force a_N and a_P to use the same
kind of numbers. That is perfect. And we do this by specifying that
the types ``a_N.number'' and ``a_P.number'' must be the same. *)
functor NewPlusOverJ (structure a_N: NUMBERS_WITH_REVEAL_CONCEAL
		      structure a_P: PLUS_OVER_NUMBER
					 sharing type a_N.number = 
						      a_P.number)
	:> J
	=
	struct

	fun plus n m = a_N.reveal (a_P.plus (a_N.conceal n)
					    (a_N.conceal m))

	end

	   

