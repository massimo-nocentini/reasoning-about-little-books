structure LittleMLerTests =
struct

  open LittleMLer
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun assertPred pred item_to_string_fun = Assert.assertEqual 
					       pred (SExpressions.to_string item_to_string_fun)

  fun assert_pred_on_integers pred = assertPred pred Int.toString

  (* val atom_eight = Atom 8 *)
  (* val flat_integer_list = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(6) ^(7) ^(8))` *)
  (* val complex_tree_of_eights = parse `((((((^(8))) ^(8) ^(8) (^(8) ((^(8))))) ^(8) (^(8) ((^(8)) ^(8))))) ^(8))` *)
  (* val complex_tree_shuffled = parse `((((((^(1))) ^(8) ^(3) (^(4) ((^(8))))) ^(6) (^(7) ((^(8)) ^(9))))) ^(8))` *)

  (* fun eight_filter 8 = true *)
  (*   | eight_filter _ = false *)

  (* fun remove_from_atom_eight remove_fn () = *)
  (*     let  *)
  (* 	  val sut = (to_string Int.toString) o (remove_fn eight_filter)	   *)
  (*     in *)
  (* 	  (sut atom_eight; *)
  (* 	   Assert.fail "It shouldn't be allowed to removed from an atomic sexp")  *)
  (* 	  handle RemoveCannotBeAppliedToAtomicSexp => ()	   *)
  (*     end *)

  (* fun remove_from_flat_integer_list remove_fn () = *)
  (*     let  *)
  (* 	  val sut = (to_string Int.toString) o (remove_fn eight_filter) *)
  (* 	  val computed = sut flat_integer_list *)
  (*     in *)
  (* 	  Assert.assertEqualString "(1 2 3 4 5 6 7)" computed *)
  (*     end *)

  (* fun remove_from_complex_tree_of_eights remove_fn () = *)
  (*     let  *)
  (* 	  val sut = (to_string Int.toString) o (remove_fn eight_filter) *)
  (* 	  val computed = sut complex_tree_of_eights *)
  (*     in *)
  (* 	  Assert.assertEqualString "(((((()) ((()))) ((())))))" computed *)
  (*     end *)

  (* fun remove_from_complex_shuffled_tree remove_fn () = *)
  (*     let  *)
  (* 	  val sut = (to_string Int.toString) o (remove_fn eight_filter) *)
  (* 	  val computed = sut complex_tree_shuffled *)
  (*     in *)
  (* 	  Assert.assertEqualString "((((((1)) 3 (4 (()))) 6 (7 (() 9)))))" computed *)
  (*     end *)

  (* fun length_of_atom_eight length_fn () = *)
  (*     let *)
  (* 	  val computed = length_fn atom_eight *)
  (*     in *)
  (* 	  Assert.assertEqualInt 1 computed *)
  (*     end *)

  (* fun length_of_flat_integer_list length_fn () = *)

  (*     let *)
  (* 	  val computed = length_fn flat_integer_list *)
  (*     in *)
  (* 	  Assert.assertEqualInt 8 computed *)
  (*     end *)

  (* fun length_of_complex_shuffled_tree length_fn () = *)
  (*     let *)
  (* 	  val computed = length_fn complex_tree_shuffled *)
  (*     in *)
  (* 	  Assert.assertEqualInt 10 computed *)
  (*     end *)

  fun check_123_prefixing prefixer () = 
      let 
	  val computed = prefixer (parse `((^(5)) ((^(9) ^(3))) ^(0))`)
	  val expected = parse `(^(1) ^(2) ^(3) (^(5)) ((^(9) ^(3))) ^(0))`
      in 
	  assert_pred_on_integers (op =) expected computed 
      end

  fun suite () =
      Test.labelTests
      [
        ("check 123 prefix using curried slist combination strategy",
	 check_123_prefixing curried_prefixer),
        ("check 123 prefix using staged slist combination strategy",
	 check_123_prefixing staged_prefixer),
        ("check 123 prefix using waiting slist combination strategy",
	 check_123_prefixing waiting_prefixer),
        ("check 123 prefix using direct consing slist combination strategy",
	 check_123_prefixing prefixer)
      ]

end
    
    
