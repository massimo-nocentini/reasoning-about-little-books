structure SExpressionsTests =
struct

  open SExpressions
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun assertPred pred item_to_string_fun = Assert.assertEqual 
					       pred (SExpressions.to_string item_to_string_fun)

  fun assert_pred_on_integers pred = assertPred pred Int.toString

  val null_list_sexp = List Null
  val single_atom = Atom 8
  val list_with_one_level = List (Cons (Atom 3, Cons (Atom 4, Cons (Atom 5, Null))))
  val eight_wrapped_three_time = List (Cons (List (Cons (List (Cons (Atom 8, Null)), Null)), Null))
  val two_level_list = List (Cons (List (Cons (Atom 8, Null)), 
				   Cons (List (Cons (Atom 4, Null)), 
					 Cons (List (Cons (Atom 7, Null)), Null))))
  val null_inside_comb = List (Cons (Atom 8, Cons (
					 List (Cons (List (Cons (Atom 4, Null)), Cons (List (Null), Null))),
					 Cons (List (Cons (Atom 7, Null)), Null))))

  (* parse tests **************************************************************)

  fun parsing_empty_quotation_should_preduce_null_list_sexp () =
      let 
	  val computed = parse ``
      in
	  assertPred (op =) (fn x => "hukairs") null_list_sexp computed
      end

  fun parsing_open_close_parens_only_should_preduce_null_list_sexp () =
      let 
	  val computed = parse `()`
      in
	  assertPred (op =) (fn x => "hukairs") null_list_sexp computed
      end

  fun parsing_single_eight_should_produce_an_atom_with_eight_content () =
      let 
	  val computed = parse `^(8)`
      in
	  assert_pred_on_integers (op =) single_atom computed
      end

  fun parsing_list_with_one_level () =
      let 
	  val computed = parse `(^(3) ^(4) ^(5))`
      in
	  assert_pred_on_integers (op =) list_with_one_level computed
      end

  fun parsing_eight_wrapped_three_times () =
      let 
	  val computed = parse `(((^(8))))`
      in
	  assert_pred_on_integers (op =) eight_wrapped_three_time computed
      end

  fun parsing_two_level_list () =
      let 
	  val computed = parse `( (^(8)) (^(4)) (^(7)) )`
      in
	  assert_pred_on_integers (op =) two_level_list computed
      end

  fun parsing_null_inside_comb () =
      let 
	  val computed = parse `( ^(8) ((^(4)) ()) (^(7)) )`
      in
	  assert_pred_on_integers (op =) null_inside_comb computed
      end

  (* to_string tests **************************************************************)

  val parse_and_to_string = (SExpressions.to_string Int.toString) o parse

  fun to_string_empty_quotation_should_preduce_null_list_sexp () =
      let 
	  val computed = parse_and_to_string ``
      in
	  Assert.assertEqualString "()" computed
      end

  fun to_string_open_close_parens_only_should_preduce_null_list_sexp () =
      let 
	  val computed = parse_and_to_string `()`
      in
	  Assert.assertEqualString "()" computed
      end

  fun to_string_single_eight_should_produce_an_atom_with_eight_content () =
      let 
	  val computed = parse_and_to_string `^(8)`
      in
	  Assert.assertEqualString "8" computed
      end

  fun to_string_list_with_one_level () =
      let 
	  val computed = parse_and_to_string `(^(3) ^(4) ^(5))`
      in
	  Assert.assertEqualString "(3 4 5)" computed
      end

  fun to_string_eight_wrapped_three_times () =
      let 
	  val computed = parse_and_to_string `(((^(8))))`
      in
	  Assert.assertEqualString "(((8)))" computed
      end

  fun to_string_two_level_list () =
      let 
	  val computed = parse_and_to_string `( (^(8)) (^(4)) (^(7)) )`
      in
	  Assert.assertEqualString "((8) (4) (7))" computed
      end

  fun to_string_null_inside_comb () =
      let 
	  val computed = parse_and_to_string `( ^(8) ((^(4)) ()) (^(7)) )`
      in
	  Assert.assertEqualString "(8 ((4) ()) (7))" computed
      end

  fun atomp_of_atom_should_return_true () =
      let
	  val computed = is_atom single_atom
      in
	  Assert.assertTrue computed
      end

  fun atomp_of_empty_list_should_return_false () =
      let
	  val computed = is_atom null_list_sexp
      in
	  Assert.assertFalse computed
      end

  fun suite () =
      Test.labelTests
      [
        ("parsing empty quotation", parsing_empty_quotation_should_preduce_null_list_sexp),
        ("parsing open-close parens only", parsing_open_close_parens_only_should_preduce_null_list_sexp),
	("parsing atom 8", parsing_single_eight_should_produce_an_atom_with_eight_content),
	("parsing a list with one livel of conses", parsing_list_with_one_level),
	("parsing eight wrapped three times", parsing_eight_wrapped_three_times),
	("parsing two level list", parsing_two_level_list),
	("parsing: null inside combination at various level", parsing_null_inside_comb),
        ("to_string empty quotation", to_string_empty_quotation_should_preduce_null_list_sexp),
        ("to_string open-close parens only", to_string_open_close_parens_only_should_preduce_null_list_sexp),
	("to_string atom 8", to_string_single_eight_should_produce_an_atom_with_eight_content),
	("to_string a list with one livel of conses", to_string_list_with_one_level),
	("to_string eight wrapped three times", to_string_eight_wrapped_three_times),
	("to_string two level list", to_string_two_level_list),
	("to string: null inside combination at various level", to_string_null_inside_comb),
	("atomp of an atom should return true", atomp_of_atom_should_return_true),
	("atomp of null list should return false", atomp_of_empty_list_should_return_false)
      ]

end
    
