structure SeasonedSchemerTests =
struct

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  structure SexpStr = MakeSexp ()
  structure SeasonedSchemerStr = SeasonedSchemer (
      structure SexpStr = SexpStr)

  structure SexpFunctions = SexpFunctionsStandardImpl (
      structure Sexp = SexpStr)

  open SexpStr SeasonedSchemerStr


  fun assertPred pred item_to_string_fun = Assert.assertEqual 
					       pred (to_string item_to_string_fun)

  fun assert_pred_on_integers pred = assertPred pred Int.toString

  val atom_eight = Atom 8
  val sexp_without_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(6) ^(7) ^(8))`
  val sexp_with_an_atom_in_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(5) ^(7) ^(8))`
  val sexp_with_a_sexp_in_two_in_a_row = parse `((^(4)) (^(3) (^(2))) (^(3) (^(2))) ^(9))`

  fun eight_filter 8 = true
    | eight_filter _ = false

  fun test_two_in_a_row two_in_a_row_fn () =
      let 
	  val sut = two_in_a_row_fn (fn fst => fn snd => fst = snd)	  
      in
	  Assert.assertFalse (sut atom_eight);
	  Assert.assertFalse (sut sexp_without_two_in_a_row);
	  Assert.assertTrue (sut sexp_with_an_atom_in_two_in_a_row);
	  Assert.assertTrue (sut sexp_with_a_sexp_in_two_in_a_row)
      end

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

  fun suite () =
      Test.labelTests
      [
        ("test_two_in_a_row of first version: use `is_first_in' helper",
	 test_two_in_a_row two_in_a_row_using_helper_function),

        ("test_two_in_a_row of second version: leaving decision to recur to `is_first_in' helper",
	 test_two_in_a_row two_in_a_row_leaving_recursion_to_helper),

        ("test_two_in_a_row of second version: two_in_a_row_recursion_only_through_helper",
	 test_two_in_a_row two_in_a_row_recursion_only_through_helper),

	("test_sum_of_prefixes", test_sum_of_prefixes),

	("test_scramble", test_scramble)

	


      ]

end
    
