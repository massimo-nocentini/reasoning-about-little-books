structure MutableStorageTests =
struct

	structure Assert = SMLUnit.Assert
	structure Test = SMLUnit.Test

	structure SexpStr = MakeSexp ()

	structure SexpParser = SExpParserSMLofNJ (
			structure Sexp = SexpStr) 

	val curried_equal = fn (fst: int ) => fn snd => fst = snd

	structure SexpEqualFunction = SexpEqual (structure Sexp = SexpStr)

	open SexpStr 
	open SexpParser

	local
		structure SexpToStringFunction = SexpToString (
				structure Sexp = SexpStr)
	in
		fun assertPred pred item_to_string_fun = Assert.assertEqual 
			pred (SexpToStringFunction.to_string item_to_string_fun)

		fun assert_pred_on_integers pred = assertPred pred Int.toString
	end

	val atom_eight = Atom 8
	val sexp_without_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(6) ^(7) ^(8))`
	val sexp_with_an_atom_in_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(5) ^(7) ^(8))`
	val sexp_with_a_sexp_in_two_in_a_row = parse `((^(4)) (^(3) (^(2))) (^(3) (^(2))) ^(9))`

	fun eight_filter 8 = true
	|	eight_filter _ = false

    local
        datatype ingredients = Chocolate | Fruit | Cheese | Carrot | Cake

        structure ConsingOntoFixElementRememberingArgumentsFunction = 
            ConsingOntoFixElementRememberingArguments(
                type t = ingredients
                val fixed_element = Cake)
    in 
        fun test_sweet_tooth_remember () = 
            let 
                open ConsingOntoFixElementRememberingArgumentsFunction
                
                val {   result = [Chocolate, Cake], 
                        arguments_seen_so_far = [Chocolate] } = cons Chocolate
                val {   result = [Fruit, Cake], 
                        arguments_seen_so_far = [Fruit, Chocolate] } = cons Fruit
                val {   result = [Cheese, Cake], 
                        arguments_seen_so_far = [Cheese, Fruit, Chocolate] } = cons Cheese
                val {   result = [Carrot, Cake],
                        arguments_seen_so_far = [Carrot, Cheese, Fruit, Chocolate] } = cons Carrot
                val {   result = [Chocolate, Cake], 
                        arguments_seen_so_far = [Chocolate, Carrot, Cheese, Fruit, Chocolate] } = cons Chocolate
            in () end
    end

	fun suite () = Test.labelTests [
        ("test_sweet_tooth_remember", test_sweet_tooth_remember)
	]

	end

