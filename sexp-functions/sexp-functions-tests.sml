structure SexpFunctionsTests =
struct

structure Assert = SMLUnit.Assert
structure Test = SMLUnit.Test

structure SexpStr = MakeSexp ()

structure SexpParser = SExpParserSMLofNJ (
				structure Sexp = SexpStr)

structure SexpToStringFunction = SexpToString(
				structure Sexp = SexpStr)

structure SexpCombineCurriedFunction = CombineSexpCurried(structure Sexp = SexpStr)
structure SexpCombineStagedFunction = CombineSexpStaged(structure Sexp = SexpStr)
structure SexpPickFunction = SexpPick (structure Sexp = SexpStr)

  open SexpStr 
  open SexpParser 

  fun assertPred pred item_to_string_fun = 
      Assert.assertEqual pred (SexpToStringFunction.to_string item_to_string_fun)

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


	(*
		We choose not to provide an implementation for `is_atom'
		function since it inhibits a good use of pattern matching.

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
	*)

  fun combining_two_empty_lists_should_return_an_empty_list combine () =
      let
	  val computed = combine null_list_sexp null_list_sexp
      in
		assert_pred_on_integers (op =) null_list_sexp computed 
      end

  fun combining_an_empty_list_with_an_atom_should_put_that_atom_in_a_list combine () = 
      let	  
	  val computed = combine null_list_sexp (Atom 5)

	  (* here we use ``parse'' assuming, by induction on this test
	  suite, that ``parse'' behaves correctly *)
	  val expected = parse `(^(5))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_two_atoms_should_build_a_flat_list_containing_them combine () = 
      let 
	  val computed = combine (Atom 4) (Atom 2)
	  val expected = parse `(^(4) ^(2))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_a_list_with_an_atom_should_build_a_list_with_the_former_list_in_car_and_a_list_for_the_cdr combine () = 
      let 
	  val computed = combine (parse `(((^(2))) ^(4) (^(9)))`) (Atom 3)
	  val expected = parse `(((^(2))) ^(4) (^(9)) ^(3))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_a_non_empty_list_with_an_empty_list_should_put_the_former_non_empty_list_in_a_list combine () = 
      let 
	  val non_empty_list = parse `(((^(2))) ^(4) (^(9)))`
	  val computed = combine non_empty_list null_list_sexp
	  val expected = non_empty_list
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_an_empty_list_with_a_non_empty_list_should_return_the_latter_non_empty_list combine () = 
      let 
	  val non_empty_list = parse `(((^(2))) ^(4) (^(9)))`
	  val computed = combine null_list_sexp non_empty_list
	  val expected = non_empty_list
      in
	  assert_pred_on_integers (op =) expected computed
      end	  

  fun combining_an_atom_with_an_empty_list_should_build_one_element_list_with_that_atom combine () = 
      let 
	  val computed = combine (Atom 4) null_list_sexp
	  val expected = parse `(^(4))`
      in
	  assert_pred_on_integers (op =) expected computed
      end

  fun combining_two_non_empty_lists_should_build_a_new_list_containing_their_elements combine () = 
      let 
	  val computed = combine 
				      (parse `((((^(3)) ^(8) ^(1)) ^(9)) ^(0))`) 
				      (parse `(^(2) ((^(8)) ((((^(1))) ^(3))) ^(2)))`)
	  val expected = parse `((((^(3)) ^(8) ^(1)) ^(9)) ^(0) ^(2) ((^(8)) ((((^(1))) ^(3))) ^(2)))`
      in
	  assert_pred_on_integers (op =) expected computed
      end      

  fun test_pick () = 
      let
	  val Atom should_be_one = SexpPickFunction.pick (parse `(^(4) ^(3) ^(1) ^(1) ^(1))`) 4
	  val Atom should_be_three = SexpPickFunction.pick (parse `(^(4) ^(3) ^(1) ^(1) ^(1))`) 2
      in
	  Assert.assertEqualInt 1 should_be_one;
	  Assert.assertEqualInt 3 should_be_three
      end

  fun suite () =
      Test.labelTests
	  [

	  (*
	   ("atomp of an atom should return true", atomp_of_atom_should_return_true),
	   ("atomp of null list should return false", atomp_of_empty_list_should_return_false),
	   *)
	    ("combining two empty lists should return an empty list using curried slist combination strategy", 
	     combining_two_empty_lists_should_return_an_empty_list SexpCombineCurriedFunction.combine),
	    ("combining an empty list with an atom should build a one-element list containing that atom using curried slist combination strategy",
	     combining_an_empty_list_with_an_atom_should_put_that_atom_in_a_list SexpCombineCurriedFunction.combine),
	    ("combining two atoms should build a flat list containing them using curried slist combination strategy",
	     combining_two_atoms_should_build_a_flat_list_containing_them SexpCombineCurriedFunction.combine),
	    ("combining a list with an atom should build a list with the former list in car and a list for the cdr using curried slist combination strategy",
	     combining_a_list_with_an_atom_should_build_a_list_with_the_former_list_in_car_and_a_list_for_the_cdr SexpCombineCurriedFunction.combine),
	    ("combining a non empty list with an empty list should put the former non empty list in a list using curried slist combination strategy",
	     combining_a_non_empty_list_with_an_empty_list_should_put_the_former_non_empty_list_in_a_list SexpCombineCurriedFunction.combine),
	    ("combining an empty list with a non empty list should return the latter non empty list using curried slist combination strategy",
	     combining_an_empty_list_with_a_non_empty_list_should_return_the_latter_non_empty_list SexpCombineCurriedFunction.combine),
	    ("combining an atom with an empty list should build one element list with that atom using curried slist combination strategy",
	     combining_an_atom_with_an_empty_list_should_build_one_element_list_with_that_atom SexpCombineCurriedFunction.combine),
	    ("combining two non empty lists should build a new list containing their elements using curried slist combination strategy",
	     combining_two_non_empty_lists_should_build_a_new_list_containing_their_elements SexpCombineCurriedFunction.combine),

	    ("combining two empty lists should return an empty list using staged slist combination strategy", 
	     combining_two_empty_lists_should_return_an_empty_list SexpCombineStagedFunction.combine),
	    ("combining an empty list with an atom should build a one-element list containing that atom using staged slist combination strategy",
	     combining_an_empty_list_with_an_atom_should_put_that_atom_in_a_list SexpCombineStagedFunction.combine),
	    ("combining two atoms should build a flat list containing them using staged slist combination strategy",
	     combining_two_atoms_should_build_a_flat_list_containing_them SexpCombineStagedFunction.combine),
	    ("combining a list with an atom should build a list with the former list in car and a list for the cdr using staged slist combination strategy",
	     combining_a_list_with_an_atom_should_build_a_list_with_the_former_list_in_car_and_a_list_for_the_cdr SexpCombineStagedFunction.combine),
	    ("combining a non empty list with an empty list should put the former non empty list in a list using staged slist combination strategy",
	     combining_a_non_empty_list_with_an_empty_list_should_put_the_former_non_empty_list_in_a_list SexpCombineStagedFunction.combine),
	    ("combining an empty list with a non empty list should return the latter non empty list using staged slist combination strategy",
	     combining_an_empty_list_with_a_non_empty_list_should_return_the_latter_non_empty_list SexpCombineStagedFunction.combine),
	    ("combining an atom with an empty list should build one element list with that atom using staged slist combination strategy",
	     combining_an_atom_with_an_empty_list_should_build_one_element_list_with_that_atom SexpCombineStagedFunction.combine),
	    ("combining two non empty lists should build a new list containing their elements using staged slist combination strategy",
	     combining_two_non_empty_lists_should_build_a_new_list_containing_their_elements SexpCombineStagedFunction.combine),

	    ("test_pick", test_pick)


	  ]

end
    
    
    
