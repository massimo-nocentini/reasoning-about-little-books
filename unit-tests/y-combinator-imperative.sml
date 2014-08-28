structure YcombinatorImperativeTests =
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

    structure SexpToStringFunction = SexpToString ( structure Sexp = SexpStr)

    fun assertPred pred item_to_string_fun = Assert.assertEqual 
        pred (SexpToStringFunction.to_string item_to_string_fun)

    fun assert_pred_on_integers pred = assertPred pred Int.toString

	val atom_eight = Atom 8
	val sexp_without_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(6) ^(7) ^(8))`
	val sexp_with_an_atom_in_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(5) ^(7) ^(8))`
	val sexp_with_a_sexp_in_two_in_a_row = parse `((^(4)) (^(3) (^(2))) (^(3) (^(2))) ^(9))`

	fun eight_filter 8 = true
	|	eight_filter _ = false


	fun suite () = Test.labelTests [
	]

	end

