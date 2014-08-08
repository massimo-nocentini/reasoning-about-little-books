structure SeasonedSchemerTests =
struct

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  structure SexpStr = MakeSexp ()

  structure SexpParser = SExpParserSMLofNJ (
                  structure Sexp = SexpStr) 

  structure SeasonedSchemerStr = SeasonedSchemer (
		  structure SexpStr = SexpStr)

  structure SexpToStringFunction = SexpToString (
		  structure Sexp = SexpStr)

        val curried_equal = fn (fst: int ) => fn snd => fst = snd

        structure SexpEqualFunction = SexpEqual (
                        structure Sexp = SexpStr)
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

  open SexpStr 
  open SexpParser
  open SeasonedSchemerStr

  fun assertPred pred item_to_string_fun = Assert.assertEqual 
        pred (SexpToStringFunction.to_string item_to_string_fun)

  fun assert_pred_on_integers pred = assertPred pred Int.toString

  val atom_eight = Atom 8
  val sexp_without_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(6) ^(7) ^(8))`
  val sexp_with_an_atom_in_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(5) ^(7) ^(8))`
  val sexp_with_a_sexp_in_two_in_a_row = parse `((^(4)) (^(3) (^(2))) (^(3) (^(2))) ^(9))`

  fun eight_filter 8 = true
    | eight_filter _ = false

  fun test_two_in_a_row  () =
        (
         Assert.assertFalse (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row   atom_eight curried_equal);
         Assert.assertFalse (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row        atom_eight curried_equal);
         Assert.assertFalse (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row      atom_eight curried_equal);

         Assert.assertFalse (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row   sexp_without_two_in_a_row curried_equal);
         Assert.assertFalse (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row        sexp_without_two_in_a_row curried_equal);
         Assert.assertFalse (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row      sexp_without_two_in_a_row curried_equal);

         Assert.assertTrue (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row   sexp_with_an_atom_in_two_in_a_row curried_equal);
         Assert.assertTrue (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row        sexp_with_an_atom_in_two_in_a_row curried_equal);
         Assert.assertTrue (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row      sexp_with_an_atom_in_two_in_a_row curried_equal);

         Assert.assertTrue (SexpTwoInARowWithIndependentHelperFunction.two_in_a_row   sexp_with_a_sexp_in_two_in_a_row curried_equal);
         Assert.assertTrue (SexpTwoInARowLeavingRecursionToHelperFunction.two_in_a_row        sexp_with_a_sexp_in_two_in_a_row curried_equal);
         Assert.assertTrue (SexpTwoInARowRecursionOnlyThroughHelperFunction.two_in_a_row      sexp_with_a_sexp_in_two_in_a_row curried_equal))

  fun test_sum_of_prefixes () = 
      let
	  val computed_mixture = sum_of_prefixes_unprotected (parse `(^(2) ^(1) ^(9) ^(17) ^(0))`)
	  val expected_mixture = parse `(^(2) ^(3) ^(12) ^(29) ^(29))`

	  val computed_ones = sum_of_prefixes_unprotected (parse `(^(1) ^(1) ^(1) ^(1) ^(1))`)
	  val expected_ones = parse `(^(1) ^(2) ^(3) ^(4) ^(5))`
      in
	  assertPred (op =) Int.toString computed_mixture expected_mixture;
	  assertPred (op =) Int.toString computed_ones expected_ones
      end

  fun test_scramble () = 
      let
	  val first_computed = scramble_unprotected 
				   (parse `(^(1) ^(1) ^(1) ^(3) ^(4) ^(2) ^(1) ^(1) ^(9) ^(2))`)
	  val first_expected = parse `(^(1) ^(1) ^(1) ^(1) ^(1) ^(4) ^(1) ^(1) ^(1) ^(9))`
      in
	  assertPred (op =) Int.toString  first_expected first_computed
      end

  fun test_multirember () =
      let
	  val element_not_present = multirember (Atom 8)
					(parse `(^(1) ^(1) ^(1) ^(3) ^(4) ^(2) ^(1) ^(1) ^(9) ^(2))`)
	  val same_list_expected = parse `(b^(1) ^(1) ^(1) ^(3) ^(4) ^(2) ^(1) ^(1) ^(9) ^(2))`
      in 
	  assertPred (op =) Int.toString same_list_expected element_not_present
      end

  fun suite () =
      Test.labelTests
      [
        ("test_two_in_a_row of first version: use `is_first_in' helper", test_two_in_a_row ),

	("test_sum_of_prefixes", test_sum_of_prefixes),

	("test_scramble", test_scramble),

	("test_multirember", test_multirember)

	


      ]

end
    
