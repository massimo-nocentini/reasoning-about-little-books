
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


functor MakeTable' = MakeTableWithStringIdentifierAndDoubleListImpl;
structure Table = MakeTableWithStringIdentifierAndDoubleListImpl ();
datatype food = Pate | Tomato | Pomodoro
val anEntry = Table.new_entry ["appetizer", "entree"] 
			      [Pate, Pomodoro];

