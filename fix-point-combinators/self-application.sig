


signature SELF_APPLICATION = 
sig
    datatype 'a T = Into of 'a T -> 'a
end

signature APPLICATION_ARIETY =
sig
    type 'a T
    type ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	  'k) ariety

    val G: ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	  'k) ariety T ->
	   ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	  'k) ariety
end

signature FIX_POINT_COMBINATOR = 
sig

    type 'a T
    type ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	  'k) ariety

    (* val fix: (('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j ,  *)
    (* 	       'k) ariety T -> 'l) -> *)
    (* 	     ('l -> ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j ,  *)
    (* 		     'k) ariety) -> *)
    (* 	     ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j ,  *)
    (* 	      'k) ariety *)

    val fix: (('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	       'k) ariety T -> 
	      ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	       'k) ariety) ->
	     (('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	       'k) ariety -> ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
		     'k) ariety) ->
	     ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	      'k) ariety


end

functor Y_DerivedFrom_LittleLisper (
    structure SelfApplication : SELF_APPLICATION
    structure Ariety: APPLICATION_ARIETY
			  sharing type SelfApplication.T = 
				       Ariety.T) 
	: FIX_POINT_COMBINATOR = 
	struct
	
	type 'a T = 'a SelfApplication.T
	type ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
 	      'k) ariety = 
	     ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
 	      'k) Ariety.ariety

	val fix = 
	 fn G => 
	    fn M => 
	       (fn (future as (SelfApplication.Into _)) => M (G future)) 
		   (SelfApplication.Into (
			 fn (future as (SelfApplication.Into _)) =>
			    M (G future)))	
	end

functor ArietyZero (
    structure SelfApplication : SELF_APPLICATION)
	: APPLICATION_ARIETY = 
	struct

	type 'a T = 'a SelfApplication.T
	type ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	      'k) ariety = unit -> 'a

	fun G (into as SelfApplication.Into aFn) () =
	    aFn into ()

	end

functor ArietyOne (
    structure SelfApplication : SELF_APPLICATION)
	: APPLICATION_ARIETY = 
	struct

	type 'a T = 'a SelfApplication.T
	type ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	      'k) ariety = 'a -> 'b

	fun G (into as SelfApplication.Into aFn) x1 =
	    aFn into x1

	end

(* the following seems superflous but I do not understand upto now... *)
functor ArietyTwo (
    structure SelfApplication : SELF_APPLICATION)
	: APPLICATION_ARIETY = 
	struct

	type 'a T = 'a SelfApplication.T
	type ('a , 'b , 'c , 'd , 'e , 'f , 'g , 'h , 'i , 'j , 
	      'k) ariety = 'a -> 'b -> 'c

	fun G (into as SelfApplication.Into aFn) x1 x2 =
	    aFn into x1 x2

	end

(* signature TWO_ARGUMENTS = *)
(* sig *)
(*     type 'a T *)
(*     type ('a,'b,'c) two_arguments *)

(*     val G: ('a,'b,'c) two_arguments T -> ('a,'b,'c) two_arguments *)
(* end *)

(* functor TwoArgumentSelfApplication ( *)
(*     structure SelfApplication : SELF_APPLICATION) *)
(* 	: TWO_ARGUMENTS  *)
(* 	       (* where type 'a T = 'a SelfApplication.T *) *)
(* 	       (* where type ('a,'b,'c) two_argument = 'a -> 'b -> 'c *) *)
(*         = *)
(* 	struct *)

(* 	type 'a T = 'a SelfApplication.T *)
(* 	type ('a,'b,'c) two_arguments = 'a -> 'b -> 'c *)

(* 	fun G (into as SelfApplication.Into aFn) x1 x2 = *)
(* 	    aFn into x1 x2 *)

(* 	end *)

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
