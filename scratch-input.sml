
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
structure SIE = SchemeInterpreterEnvironment (structure aParser = SexpParser)
structure Interpreter = SIE.MakeInterpreter ()

open SIE Interpreter;
(* the following doesn't compile, see the compiler complaint: *)
(* val anEntry = Table.new_entry [4, 5] *)
(* 			      [Pate, Pomodoro]; *)
 (* operator domain: Table.identifier Table.keys *)
 (*  operand:         int list *)
 (*  in expression: *)
 (*    Table.new_entry (4 :: 5 :: nil) *)
val sexp = SIE.Parser.parse `(^(TmQuote) (^(TmBoolean true) ^(TmInteger 5)))`;
val valu = Interpreter.value sexp;
val sexp = SIE.Parser.parse `(^(TmQuote) ^(TmInteger 5))`;
val valu = Interpreter.value sexp;
val sexp = SIE.Parser.parse `(^(TmQuote) ^(TmIdentifier "x"))`;
val valu = Interpreter.value sexp;
val sexp = SIE.Parser.parse `(^(TmCar) (^(TmQuote) (^(TmBoolean true) ^(TmInteger 5))))`;
val valu = Interpreter.value sexp;


