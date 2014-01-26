
open LittleSchemer;
val into = Into G;


open ChainLittleSchemer;

val third_integer = get_nth 3 (fn i => i + 1) 0;
val twenty_four = 
    let 
	val less_than_twenty = (fn i => i > 20)
	val increment_by_four = (fn i => i + 4)
	val starting_from_zero = 0
    in
	until less_than_twenty increment_by_four starting_from_zero
    end

val ninetyth_integer_using_ints = chain_item 90 (ints 0)
val sixth_fib_number = chain_item 6 (fibs 0 1)
val sixth_fib_number_using_nth = get_nth 6 (fn (n,m) => (m, n+m)) (0,1)
val sixth_fib_number_using_nth_projecting = ((fn (n,m) => m) o (get_nth 6 (fn (n,m) => (m, n+m))))  (0,1)
												    
open LittleMLer;

val zero = num_plus Zero Zero;
functor aFunctor = NumberAsInt;


functor MakeTable' = MakeTableDoubleListImpl;

datatype food = Pate | Tomato | Pomodoro
functor MakeTypeFood () :> TYPE where type aType = food
   =
   struct 
   type aType = food
   end

structure Table = MakeTableDoubleListImpl (
    structure IdentifierType = MakeTypeString ()
    structure StuffType = MakeTypeFood ());

val f = Table.extend_table;

val anEntry = Table.new_entry ["hello", "world"] 
			      [Pate, Pomodoro];

structure FoodSexp = MakeSexp (structure ObjectType = MakeTypeFood());
val FoodSexp.List aList = FoodSexp.List (
	FoodSexp.Cons (FoodSexp.Atom Tomato, FoodSexp.Null))

structure SexpStr = MakeSexp ()
structure SexpParser = SExpParserSMLofNJ (structure aSexp = SexpStr)
structure SIE = SchemeInterpreterEnvironment (structure Sexp = SexpStr)
structure Interpreter = SIE.MakeInterpreter ();
structure SexpFunctions = SexpFunctionsStandardImpl (structure Sexp = SexpStr)

open SIE Interpreter;
(* the following doesn't compile, see the compiler complaint: *)
(* val anEntry = Table.new_entry [4, 5] *)
(* 			      [Pate, Pomodoro]; *)
 (* operator domain: Table.identifier Table.keys *)
 (*  operand:         int list *)
 (*  in expression: *)
 (*    Table.new_entry (4 :: 5 :: nil) *)
val sexp = SexpParser.parse `(^(TmQuote) (^(TmBoolean true) ^(TmInteger 5)))`;
val valu = Interpreter.value sexp;
val sexp = SexpParser.parse `(^(TmQuote) ^(TmInteger 5))`;
val valu = Interpreter.value sexp;
val sexp = SexpParser.parse `(^(TmQuote) ^(TmIdentifier "x"))`;
val valu = Interpreter.value sexp;
val sexp = SexpParser.parse `(^(TmCar) (^(TmQuote) (^(TmBoolean true) ^(TmInteger 5))))`;
val valu = Interpreter.value sexp;
val sexp = SexpParser.parse `(^(TmCdr) (^(TmQuote) (^(TmBoolean true) ^(TmInteger 5))))`;
val valu = Interpreter.value sexp;
val sexp = SexpParser.parse `(^(TmSucc) (^(TmQuote) ^(TmInteger 5)))`;
val valu = Interpreter.value sexp;
val sexp = SexpParser.parse `(^(TmZero_p) (^(TmQuote) ^(TmInteger 5)))`;
val valu = Interpreter.value sexp;

val Y = SexpParser.parse `(^(TmLambda) (^(TmIdentifier "le"))
			  	       ((^(TmLambda) (^(TmIdentifier "f")) (^(TmIdentifier "f") ^(TmIdentifier "f")))
					    (^(TmLambda) (^(TmIdentifier "f"))
						(^(TmIdentifier "le") (^(TmLambda) (^(TmIdentifier "x")) 
							((^(TmIdentifier "f") ^(TmIdentifier "f")) ^(TmIdentifier "x")))))))`;
val Y_as_string = SexpFunctions.to_string SIE.term_to_string Y 

(* the following example shows that  *)
signature SIMPLE_SEXP = 
sig
    type 'a ssexp
    type 'b sslist
end

functor MakeSSexp () 
	=
	struct

	datatype 'a sslist = Null
			   | Cons of 'a ssexp * 'a sslist
	     and 'b ssexp = Atom of 'b
			  | List of 'b sslist

	end

structure SimpleSSexp: SIMPLE_SEXP = MakeSSexp () 
(* We cannot write the following, since Null value isn't exposed in
the signature. Abscribing a signature only has to do with types, not
with exposed values! *)
(* val _ = SimpleSSexp.Null; *)


structure SimpleSSexp = MakeSSexp () 
(* here instead we can use the values introduced by the datatype
definition. *)
val _ = SimpleSSexp.Null;

(* signature EXTENDED_SIMPLE_SEXP =  *)
(* sig *)
(*     include SIMPLE_SEXP *)
(* 	datatype 'a ssslist = Null *)
(* 			   | Cons of 'a sssexp * 'a ssslist *)
(* 	     and 'b sssexp = Atom of 'b *)
(* 			  | List of 'b ssslist *)

(* end *)

(* structure ExtendedSimpleSexp: EXTENDED_SIMPLE_SEXP = MakeSSexp () ; *)
(* val _ = fn a => (case a of ExtendedSimpleSexp.Null => 4); *)

signature DIFF_INST_TYPE = 
sig
    type 'a sexp

    val zip: 'a sexp -> 'b sexp -> ('a * 'b) sexp
end

signature GENERIC_TYPE = 
sig
    type 'a aType
end

(* functor ZipFunction (structure Sexp: SIMPLE_SEXP *)
(* 		     val firstSexp: 'a Sexp.ssexp *)
(* 		     val secondSexp: 'b Sexp.ssexp) :> GENERIC_TYPE where type 'c aType = ('a * 'b) Sexp.ssexp = *)
(* 	struct *)
	
(* 	type 'c aType = ('a * 'b) Sexp.ssexp *)

(* 	end *)
