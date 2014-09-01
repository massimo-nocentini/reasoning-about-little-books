structure SeasonedSchemerTests =
struct

	structure Assert = SMLUnit.Assert
	structure Test = SMLUnit.Test

	structure SexpStr = MakeSexp ()

	structure SexpParser = SExpParserSMLofNJ (
			structure Sexp = SexpStr) 


	val curried_equal = fn (fst: int ) => fn snd => fst = snd

	structure SexpEqualFunction = SexpEqual (structure Sexp = SexpStr)
	(* 
	 The following version is much more interesting 
	 than the following one, but we cannot make it type checks. 
	 *)
	(* 
	structure SexpTwoInARowWithIndependentHelperFunction =
			SexpTwoInARowWithIndependentHelper(
					type elem = int
					type 'a t = int SexpStr.sexp
					structure Sexp = SexpStr
					structure SexpEqualFunction = SexpEqualFunction
					val comparer = curried_equal)
	*)

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
		structure SexpTwoInARowWithIndependentHelperFunction =
				SexpTwoInARowWithIndependentHelper(
						structure Sexp = SexpStr
						structure SexpEqualFunction = SexpEqualFunction)

		structure SexpTwoInARowLeavingRecursionToHelperFunction =
				SexpTwoInARowLeavingRecursionToHelper(
						structure Sexp = SexpStr
						structure SexpEqualFunction = SexpEqualFunction)

		structure SexpTwoInARowRecursionOnlyThroughHelperFunction =
				SexpTwoInARowRecursionOnlyThroughHelper(
						structure Sexp = SexpStr
						structure SexpEqualFunction = SexpEqualFunction)

	in
		fun test_two_in_a_row  () = (
			Assert.assertFalse (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row   
				atom_eight curried_equal);
			Assert.assertFalse (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row
				atom_eight curried_equal);
			Assert.assertFalse (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row
				atom_eight curried_equal);
			Assert.assertFalse (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row
				sexp_without_two_in_a_row curried_equal);
			Assert.assertFalse (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row
				sexp_without_two_in_a_row curried_equal);
			Assert.assertFalse (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row
				sexp_without_two_in_a_row curried_equal);
			Assert.assertTrue (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row
				sexp_with_an_atom_in_two_in_a_row curried_equal);
			Assert.assertTrue (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row
				sexp_with_an_atom_in_two_in_a_row curried_equal);
			Assert.assertTrue (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row
				sexp_with_an_atom_in_two_in_a_row curried_equal);
			Assert.assertTrue (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row
				sexp_with_a_sexp_in_two_in_a_row curried_equal);
			Assert.assertTrue (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row
				sexp_with_a_sexp_in_two_in_a_row curried_equal);
			Assert.assertTrue (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row
				sexp_with_a_sexp_in_two_in_a_row curried_equal))
	end

    local
        structure SexpTwoInARowStarFunction = SexpTwoInARowStar(
            structure Sexp = SexpStr)

        datatype food = Donuts | Cheerios | Spaghettios | Mozzarella | Pizza | Cake
                        | Potato | Chip | With | Fish

        val counter = ref 0
    in
        fun test_two_in_a_row_star () = 
            let 
                val two_in_a_row = SexpTwoInARowStarFunction.two_in_a_row 
                val comparer = (fn a => fn b => a = b)

                val _ = counter := !counter + 1
                val false = two_in_a_row 
                                (parse `(^(Mozzarella) ^(Cake) ^(Mozzarella))`)
                                comparer 

                val _ = counter := !counter + 1
                val false = two_in_a_row 
                                (parse `((^(Mozzarella)) (^(Cake)) ^(Mozzarella))`)
                                comparer 

                val _ = counter := !counter + 1
                val true = two_in_a_row 
                                (parse `((^(Mozzarella)) (^(Cake)) ^(Mozzarella) ((()) ((((()) ^(Mozzarella))))))`)
                                comparer 

                val _ = counter := !counter + 1
                val true = two_in_a_row 
                                (parse `((^(Potato)) (^(Chip)) ((()) ^(Chip) (^(With)) ^(Fish)))`)
                                comparer 

                val _ = counter := !counter + 1
                val false = two_in_a_row 
                                (parse `((^(Donuts)) (^(Cheerios) (() ^(Chip) (^(With)))) ^(Fish))`)
                                comparer 
            in () end

        fun check_counter_about_two_in_a_row_star () =
            let val 5 = !counter in () end
    end


	local
		structure SexpSumOfPrefixesFunction = 
			SexpSumOfPrefixes (structure Sexp = SexpStr)
	in
		fun test_sum_of_prefixes () = 
			let
				val computed_mixture = SexpSumOfPrefixesFunction.sum_of_prefixes (parse `(^(2) ^(1) ^(9) ^(17) ^(0))`)
				val expected_mixture = parse `(^(2) ^(3) ^(12) ^(29) ^(29))` 
				val computed_ones = SexpSumOfPrefixesFunction.sum_of_prefixes (parse `(^(1) ^(1) ^(1) ^(1) ^(1))`)
				val expected_ones = parse `(^(1) ^(2) ^(3) ^(4) ^(5))`
			in
				assertPred (op =) Int.toString computed_mixture expected_mixture;
				assertPred (op =) Int.toString computed_ones expected_ones
			end
	end

	local
		structure SexpPickFunction = SexpPick (
				structure Sexp = SexpStr)

		structure SexpScrambleUnprotectedFunction = SexpScramble (
			structure Sexp = SexpStr
			structure SexpPickFunction = SexpPickFunction)
	in
		fun test_scramble () = 
			let	
				val first_computed = SexpScrambleUnprotectedFunction.scramble 
					(parse `(^(1) ^(1) ^(1) ^(3) ^(4) ^(2) ^(1) ^(1) ^(9) ^(2))`)

				val first_expected = parse `(^(1) ^(1) ^(1) ^(1) ^(1) ^(4) ^(1) ^(1) ^(1) ^(9))`
			in assertPred (op =) Int.toString  first_expected first_computed end
	end

	local
		structure SexpMultiremberFunction =
				SexpMultirember(
						structure Sexp = SexpStr
						structure SexpEqualFunction = SexpEqualFunction)
	in
		fun test_multirember () =
			let
				val first_sexp 	= parse `(^(1) ^(1) ^(1) ^(3) ^(4) ^(2) ^(1) ^(1) ^(9) ^(2))`
				val first_sexp_without_ones	= parse `(^(3) ^(4) ^(2) ^(9) ^(2))`
			in 
				((* Extract both assertions in a test function that consumes multirember function under test. *)
				 assertPred (op =) Int.toString first_sexp 
					 (SexpMultiremberFunction.multirember first_sexp (Atom 8) curried_equal);
				 assertPred (op =) Int.toString first_sexp_without_ones 
					 (SexpMultiremberFunction.multirember first_sexp (Atom 1) curried_equal)) 
			end
	end

	(* Before the following we should move all the tests relative to sexp functions.  *)

	fun suite () = Test.labelTests [
		("test_two_in_a_row of first version: use `is_first_in' helper", test_two_in_a_row ),
		("test_sum_of_prefixes", test_sum_of_prefixes),
		("test_scramble", test_scramble),
		("test_multirember", test_multirember),
        ("test_two_in_a_row_star", test_two_in_a_row_star),
        ("check_counter_about_two_in_a_row_star", check_counter_about_two_in_a_row_star)

	]

	end

