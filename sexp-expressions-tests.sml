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

  fun combining_two_empty_lists_should_return_an_empty_list () =
      let
	  val computed = combine_sexp null_list_sexp null_list_sexp
      in
	  assert_pred_on_integers (op =) null_list_sexp computed	  
      end

  fun combining_an_empty_list_with_an_atom_should_put_that_atom_in_a_list () = 
      let	  
	  val computed = combine_sexp null_list_sexp (Atom 5)

	  (* here we use ``parse'' assuming, by induction on this test
	  suite, that ``parse'' behaves correctly *)
	  val expected = parse `(^(5))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_two_atoms_should_build_a_flat_list_containing_them () = 
      let 
	  val computed = combine_sexp (Atom 4) (Atom 2)
	  val expected = parse `(^(4) ^(2))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_a_list_with_an_atom_should_build_a_list_with_the_former_list_in_car_and_a_list_for_the_cdr () = 
      let 
	  val computed = combine_sexp (parse `(((^(2))) ^(4) (^(9)))`) (Atom 3)
	  val expected = parse `(((^(2))) ^(4) (^(9)) ^(3))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_a_non_empty_list_with_an_empty_list_should_put_the_former_non_empty_list_in_a_list () = 
      let 
	  val non_empty_list = parse `(((^(2))) ^(4) (^(9)))`
	  val computed = combine_sexp non_empty_list null_list_sexp
	  val expected = non_empty_list
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_an_empty_list_with_a_non_empty_list_should_return_the_latter_non_empty_list () = 
      let 
	  val non_empty_list = parse `(((^(2))) ^(4) (^(9)))`
	  val computed = combine_sexp null_list_sexp non_empty_list
	  val expected = non_empty_list
      in
	  assert_pred_on_integers (op =) expected computed
      end	  

  fun combining_an_atom_with_an_empty_list_should_build_one_element_list_with_that_atom () = 
      let 
	  val computed = combine_sexp (Atom 4) null_list_sexp
	  val expected = parse `(^(4))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_two_non_empty_lists_should_build_a_new_list_containing_their_elements () = 
      let 
	  val computed = combine_sexp (parse `((((^(3)) ^(8) ^(1)) ^(9)) ^(0))`) 
				      (parse `(^(2) ((^(8)) ((((^(1))) ^(3))) ^(2)))`)
	  val expected = parse `((((^(3)) ^(8) ^(1)) ^(9)) ^(0) ^(2) ((^(8)) ((((^(1))) ^(3))) ^(2)))`
      in
	  assert_pred_on_integers (op =) expected computed
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
	("atomp of null list should return false", atomp_of_empty_list_should_return_false),
	("combining two empty lists should return an empty list", 
	 combining_two_empty_lists_should_return_an_empty_list),
	("combining an empty list with an atom should build a one-element list containing that atom",
	combining_an_empty_list_with_an_atom_should_put_that_atom_in_a_list),
	("combining two atoms should build a flat list containing them",
	combining_two_atoms_should_build_a_flat_list_containing_them),
	("combining a list with an atom should build a list with the former list in car and a list for the cdr",
	 combining_a_list_with_an_atom_should_build_a_list_with_the_former_list_in_car_and_a_list_for_the_cdr),
	("combining a non empty list with an empty list should put the former non empty list in a list",
	 combining_a_non_empty_list_with_an_empty_list_should_put_the_former_non_empty_list_in_a_list),
	("combining an empty list with a non empty list should return the latter non empty list",
	 combining_an_empty_list_with_a_non_empty_list_should_return_the_latter_non_empty_list),
	("combining an atom with an empty list should build one element list with that atom",
	combining_an_atom_with_an_empty_list_should_build_one_element_list_with_that_atom),
	("combining two non empty lists should build a new list containing their elements",
	 combining_two_non_empty_lists_should_build_a_new_list_containing_their_elements)
      ]

end
    
    
    
