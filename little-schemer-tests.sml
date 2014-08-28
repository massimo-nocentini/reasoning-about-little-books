structure LittleSchemerTests =
struct

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  structure SexpStr = MakeSexp ()
  structure LittleSchemerStr = LittleSchemer (structure SexpStr = SexpStr)

  structure SexpToStringFunction = SexpToString (structure Sexp = SexpStr)

  open SexpStr 
  open LittleSchemerStr

  fun assertPred pred item_to_string_fun = Assert.assertEqual 
					       pred (SexpToStringFunction.to_string item_to_string_fun)

  fun assert_pred_on_integers pred = assertPred pred Int.toString

  val atom_eight = Atom 8
  val flat_integer_list = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(6) ^(7) ^(8))`
  val complex_tree_of_eights = parse `((((((^(8))) ^(8) ^(8) (^(8) ((^(8))))) ^(8) (^(8) ((^(8)) ^(8))))) ^(8))`
  val complex_tree_shuffled = parse `((((((^(1))) ^(8) ^(3) (^(4) ((^(8))))) ^(6) (^(7) ((^(8)) ^(9))))) ^(8))`

  fun eight_filter 8 = true
    | eight_filter _ = false

  fun remove_from_atom_eight remove_fn () =
      let 
	  val sut = (SexpToStringFunction.to_string Int.toString) o (remove_fn eight_filter)	  
      in
	  (sut atom_eight;
	   Assert.fail "It shouldn't be allowed to removed from an atomic sexp") 
	  handle RemoveCannotBeAppliedToAtomicSexp => ()	  
      end

  fun remove_from_flat_integer_list remove_fn () =
      let 
	  val sut = (SexpToStringFunction.to_string Int.toString) o (remove_fn eight_filter)
	  val computed = sut flat_integer_list
      in
	  Assert.assertEqualString "(1 2 3 4 5 6 7)" computed
      end

  fun remove_from_complex_tree_of_eights remove_fn () =
      let 
	  val sut = (SexpToStringFunction.to_string Int.toString) o (remove_fn eight_filter)
	  val computed = sut complex_tree_of_eights
      in
	  Assert.assertEqualString "(((((()) ((()))) ((())))))" computed
      end

  fun remove_from_complex_shuffled_tree remove_fn () =
      let 
	  val sut = (SexpToStringFunction.to_string Int.toString) o (remove_fn eight_filter)
	  val computed = sut complex_tree_shuffled
      in
	  Assert.assertEqualString "((((((1)) 3 (4 (()))) 6 (7 (() 9)))))" computed
      end

  fun length_of_atom_eight length_fn () =
      let
	  val computed = length_fn atom_eight
      in
	  Assert.assertEqualInt 1 computed
      end

  fun length_of_flat_integer_list length_fn () =
      let
	  val computed = length_fn flat_integer_list
      in
	  Assert.assertEqualInt 8 computed
      end

  fun length_of_complex_shuffled_tree length_fn () =
      let
	  val computed = length_fn complex_tree_shuffled
      in
	  Assert.assertEqualInt 10 computed
      end

  fun suite () =
      Test.labelTests
      [
        ("remove from atom eight using abridged version should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged),
        ("remove from flat list using abridged version of remove should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged),
        ("remove from complex tree of eights using abridged version of remove should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged),
        ("remove from complex shuffled tree using abridged version of remove should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged),

        ("remove from atom eight using abridged version step 1 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_1),
        ("remove from flat list using abridged version of remove step 1 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_1),
        ("remove from complex tree of eights using abridged version of remove step 1 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_1),
        ("remove from complex shuffled tree using abridged version of remove step 1 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_1),

        ("remove from atom eight using abridged version step 2 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_2),
        ("remove from flat list using abridged version of remove step 2 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_2),
        ("remove from complex tree of eights using abridged version of remove step 2 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_2),
        ("remove from complex shuffled tree using abridged version of remove step 2 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_2),

        ("remove from atom eight using abridged version step 3 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_3),
        ("remove from flat list using abridged version of remove step 3 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_3),
        ("remove from complex tree of eights using abridged version of remove step 3 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_3),
        ("remove from complex shuffled tree using abridged version of remove step 3 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_3),

        ("remove from atom eight using abridged version step 4 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_4),
        ("remove from flat list using abridged version of remove step 4 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_4),
        ("remove from complex tree of eights using abridged version of remove step 4 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_4),
        ("remove from complex shuffled tree using abridged version of remove step 4 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_4),

        ("remove from atom eight using abridged version step 5 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_5),
        ("remove from flat list using abridged version of remove step 5 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_5),
        ("remove from complex tree of eights using abridged version of remove step 5 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_5),
        ("remove from complex shuffled tree using abridged version of remove step 5 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_5),

        ("remove from atom eight using abridged version step 6 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_6),
        ("remove from flat list using abridged version of remove step 6 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_6),
        ("remove from complex tree of eights using abridged version of remove step 6 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_6),
        ("remove from complex shuffled tree using abridged version of remove step 6 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_6),

        ("remove from atom eight using abridged version step 7 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_7),
        ("remove from flat list using abridged version of remove step 7 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_7),
        ("remove from complex tree of eights using abridged version of remove step 7 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_7),
        ("remove from complex shuffled tree using abridged version of remove step 7 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_7),

        ("remove from atom eight using abridged version step 8 should raise exception",
	 remove_from_atom_eight remove_from_sexp_abridged_toward_Y_step_8),
        ("remove from flat list using abridged version of remove step 8 should produce a shorter list",
	 remove_from_flat_integer_list remove_from_sexp_abridged_toward_Y_step_8),
        ("remove from complex tree of eights using abridged version of remove step 8 should return an empty tree",
	 remove_from_complex_tree_of_eights remove_from_sexp_abridged_toward_Y_step_8),
        ("remove from complex shuffled tree using abridged version of remove step 8 should produce a shorted shuffled tree",
	 remove_from_complex_shuffled_tree remove_from_sexp_abridged_toward_Y_step_8)

      ]

end
    
