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
structure SexpEqualFunction = SexpEqual (structure Sexp = SexpStr)

structure SexpEqualWithComparerFunction = SexpEqualWithComparer (
    type t = int
    fun comparer fst snd = fst = snd
    structure Sexp = SexpStr)

(* the following is an instantiation of the abridged SexpEqual 
	structure that embeds the comparison function.

structure SexpEqualFunctionAbridged = SexpEqualAbridged(
	structure KeyEqualityComparer = KeyEqualityComparer
	type 'a sexp = 'a SexpStr.sexp
	structure Sexp = SexpStr)
*)

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
        let val Atom should_be_one = SexpPickFunction.pick (parse `(^(4) ^(3) ^(1) ^(1) ^(1))`) 4
            val Atom should_be_three = SexpPickFunction.pick (parse `(^(4) ^(3) ^(1) ^(1) ^(1))`) 2
        in
            Assert.assertEqualInt 1 should_be_one;
            Assert.assertEqualInt 3 should_be_three
        end

	local
		structure SexpMemberFunction = SexpMember (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction)

(*		structure SexpMemberWithComparerFunction = SexpMemberWithComparer (
            type 'a sexp = int SexpStr.sexp
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualWithComparerFunction) *)
	in
		fun test_member () = 
			let
				fun equality_comparer (a: int) b = a = b
				val true	= SexpMemberFunction.member 
								(parse `(^(4) ^(3) ^(1) ^(1) ^(1))`)  
								(parse `^(1)`) 
								equality_comparer
				val false 	= SexpMemberFunction.member 
								(parse `(^(4) ^(3) (^(1) ^(1) ^(1)))`)  
								(parse `^(1)`) 
								equality_comparer
				val true 	= SexpMemberFunction.member 
								(parse `(^(4) ^(3) (^(1) ^(1) ^(1)))`)  
								(parse `(^(1) ^(1) ^(1))`) 
								equality_comparer
			in () (* nothing to check here since we use pattern matching in the naming box *) end
	end

	local
		structure SexpMemberFunction = SexpMember (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction)

		structure SexpUnionFunction = SexpUnion (
			structure Sexp = SexpStr
			structure SexpMemberFunction = SexpMemberFunction)
	in 
		fun test_union () = 
			let
				fun equality_comparer (a: int) b = a = b

				val fourThreeSevenOne	= SexpUnionFunction.union 
								(parse `(^(4) ^(3) ^(1) ^(1) ^(1))`)  
								(parse `(^(7) ^(1))`) 
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								fourThreeSevenOne 
								(parse `(^(4) ^(3) ^(7) ^(1))`)  

				val identity	= SexpUnionFunction.union 
								(parse `(^(4) ^(3) ^(1) ^(1) ^(1))`)  
								(parse `(^(4) ^(3) ^(1) ^(1) ^(1))`)  
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								identity 
								(parse `(^(4) ^(3) ^(1) ^(1) ^(1))`)  
			in () end
	end

	local
		structure SexpMemberFunction = SexpMember (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction)

		structure SexpIntersectFunction = SexpIntersect (
			structure Sexp = SexpStr
			structure SexpMemberFunction = SexpMemberFunction)
	in 
		fun test_intersect () = 
			let
				fun equality_comparer (a: int) b = a = b

				val fourThreeSevenOne	= SexpIntersectFunction.intersect 
								(parse `(^(4) ^(3) ^(1))`)  
								(parse `(^(7) ^(1))`) 
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								fourThreeSevenOne 
								(parse `(^(1))`)  

				val empty	= SexpIntersectFunction.intersect 
								(parse `(^(4) ^(3) ^(1) )`)  
								(parse `(^(5) ^(9) ^(2) ^(7) )`)  
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								empty 
								(parse `()`)  
			in () end
	end

	local

		structure SexpMemberFunction = SexpMember (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction)

		structure SexpIntersectFunction = SexpIntersect (
			structure Sexp = SexpStr
			structure SexpMemberFunction = SexpMemberFunction)

		structure SexpIntersectAllFunction = SexpIntersectAll (
			structure Sexp = SexpStr
			structure SexpIntersectFunction = SexpIntersectFunction)

		structure SexpIntersectAllWithLetccFunction = SexpIntersectAllWithLetcc (
			structure Sexp = SexpStr
			structure SexpIntersectFunction = SexpIntersectFunction
			structure HopSkipAndJump = HopSkipAndJump)

		structure SexpIntersectAllWithLetccIntersectEmbeddedFunction = 
			SexpIntersectAllWithLetccIntersectEmbedded (
					structure Sexp = SexpStr
					structure SexpMemberFunction = SexpMemberFunction
					structure HopSkipAndJump = HopSkipAndJump)

		datatype strange = Three | Mango | Hamburger | Kiwi | And | Diet | Baked | Potato

	in 
		fun test_intersect_all () = 
			let
				val () = test_intersect_all_helper SexpIntersectAllFunction.intersect_all
				val () = test_intersect_all_helper SexpIntersectAllWithLetccFunction.intersect_all
				val () = test_intersect_all_helper SexpIntersectAllWithLetccIntersectEmbeddedFunction.intersect_all
			in () end
		and test_intersect_all_helper intersect_all_function  = 
			let
				fun equality_comparer (a: strange) b = a = b

				val fourThreeSevenOne	= intersect_all_function 
								(parse `( 	(^(Three) ^(Mango) 		^(And)) 
											(^(Three) ^(Kiwi) 		^(And)) 
											(^(Three) ^(Hamburger)  	))` )  
								equality_comparer

				val true = SexpEqualFunction.equal 
								equality_comparer 
								fourThreeSevenOne 
								(parse `(^(Three))`)  


				val empty	= intersect_all_function 
								(parse `( 	(^(Three) ^(Mango) 	^(And)) 
											() 
											(^(Three) ^(Diet) 	^(Hamburger)))` )  
								equality_comparer

				val true = SexpEqualFunction.equal 
								equality_comparer 
								empty 
								(parse `()`)  

				val empty_complex	= intersect_all_function 
								(parse `( 	(^(Three) ^(Mango) 		^(And)) 
											(^(Three) ^(Kiwi) 		^(And)) 
											(^(Baked) ^(Potato)) 
											(^(Three) ^(Hamburger)  	))` )  
								equality_comparer

				val true = SexpEqualFunction.equal 
								equality_comparer 
								empty_complex 
								(parse `()`)  
			in () end
	end

	local
		structure SexpRemberUptoLastFunction = SexpRemberUptoLast (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction
			structure HopSkipAndJump = HopSkipAndJump)

		datatype strange = Cookies | Chocolate | Mints |
							Caramel | Delight | Ginger |
							Snaps | Desserts | Mousse |
							Vanilla | Ice | Cream |
							German | Cake |
							More | Gingerbreadman | Chip | Brownies | NotPresent
	in 
		fun test_rember_upto_last () = 
			let
				fun equality_comparer (a: strange) b = a = b

				val fourThreeSevenOne	= SexpRemberUptoLastFunction.rember_upto_last 
								(parse `(^(Cookies) ^(Chocolate) ^(Mints)
											^(Caramel) ^(Delight) ^(Ginger) ^(Snaps)
											^(Desserts) ^(Chocolate) ^(Mousse) ^(Vanilla)
											^(Ice) ^(Cream) ^(German) ^(Chocolate) ^(Cake)
											^(More) ^(Cookies) ^(Gingerbreadman) ^(Chocolate)
											^(Chip) ^(Brownies)
											)`)  
								(parse `^(Cookies)`) 
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								fourThreeSevenOne 
								(parse `(^(Gingerbreadman) ^(Chocolate) ^(Chip) ^(Brownies))`)  

				val identity	= SexpRemberUptoLastFunction.rember_upto_last 
								(parse `(^(Cookies) ^(Chocolate) ^(Mints)
											^(Caramel) ^(Delight) ^(Ginger) ^(Snaps)
											^(Desserts) ^(Chocolate) ^(Mousse) ^(Vanilla)
											^(Ice) ^(Cream) ^(German) ^(Chocolate) ^(Cake)
											^(More) ^(Cookies) ^(Gingerbreadman) ^(Chocolate)
											^(Chip) ^(Brownies)
											)`)  
								(parse `^(NotPresent)`) 
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								identity 
								(parse `(^(Cookies) ^(Chocolate) ^(Mints)
											^(Caramel) ^(Delight) ^(Ginger) ^(Snaps)
											^(Desserts) ^(Chocolate) ^(Mousse) ^(Vanilla)
											^(Ice) ^(Cream) ^(German) ^(Chocolate) ^(Cake)
											^(More) ^(Cookies) ^(Gingerbreadman) ^(Chocolate)
											^(Chip) ^(Brownies)
											)`)  
			in () end
	end

	local
		structure SexpLeftmostFunction = SexpLeftmost (
			structure Sexp = SexpStr)

		structure SexpLeftmostWithLetccFunction = SexpLeftmostWithLetcc (
			structure Sexp = SexpStr
			structure HopSkipAndJump = HopSkipAndJump)
	in 
		fun test_leftmost () = 
			let 
				val _ = test_leftmost_helper SexpLeftmostFunction.leftmost
				val _ = test_leftmost_helper SexpLeftmostWithLetccFunction.leftmost
			in () end
		and test_leftmost_helper leftmost_fn = 
			let
				val (Atom 4) = leftmost_fn (parse `(((^(4)) ^(3)) (^(1) ^(1)))`)  
				val (Atom 4) = leftmost_fn (parse `(((^(4)) ()) () ^(1))`)  
				val (Atom 4) = leftmost_fn (parse `(((() ^(4)) ()))`)  
				val (Atom 4) = leftmost_fn (parse `(((()) ()) () ^(4))`)  
			in () end
	end

	local
		structure SexpRemberOneStarFunction = SexpRemberOneStar (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction)

		structure SexpRemberOneStarWithLetccFunction = SexpRemberOneStarWithLetcc (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction
            structure HopSkipAndJump = HopSkipAndJump)

		structure SexpRemberOneStarWithTryFunction = SexpRemberOneStarWithTry (
			structure Sexp = SexpStr
			structure SexpEqualFunction = SexpEqualFunction
            structure HopSkipAndJump = HopSkipAndJump)

		datatype strange = Swedish | Rye | French | Mustard | Salad | Turkey
	in
		fun test_rember_one_star () = 
            let 
                val _ = tester SexpRemberOneStarFunction.rember_one_star 
                val _ = tester SexpRemberOneStarWithLetccFunction.rember_one_star 
                val _ = tester SexpRemberOneStarWithTryFunction.rember_one_star 
            in () end
        and tester rember_one_star =
			let
				fun equality_comparer (a: strange) b = a = b

				val fourThreeSevenOne	= rember_one_star
								(parse `((^(Swedish) ^(Rye)) (^(French) (^(Mustard) ^(Salad) ^(Turkey))) ^(Salad))`)  
								(parse `^(Salad)`) 
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								fourThreeSevenOne 
								(parse `((^(Swedish) ^(Rye)) (^(French) (^(Mustard) ^(Turkey))) ^(Salad))`)  

				val not_present	= rember_one_star
								(parse `((^(Swedish)) ^(Rye) (^(French)))`)  
								(parse `^(Salad)`) 
								equality_comparer
				val true = SexpEqualFunction.equal 
								equality_comparer 
								not_present 
								(parse `((^(Swedish)) ^(Rye) (^(French)))`)  
			in () end
	end

	local
		structure SexpDepthStarFunction = SexpDepthStar (
			structure Sexp = SexpStr)

        structure SexpDepthStarViaImperativeYFunction = 
            SexpDepthStarViaImperativeY (
                structure Sexp = SexpStr
                structure ImperativeY = Y_imperative_one_arg ())

		datatype strange = Pickled | Peppers
	in
		fun test_depth_star () = 
            let val _ = tester SexpDepthStarFunction.depth_star
                val _ = tester SexpDepthStarViaImperativeYFunction.depth_star
            in () end
        and tester depth_fn = 
			let
				val 1 = depth_fn (parse `(^(Pickled) ^(Peppers) ^(Peppers) ^(Pickled))`)
				val 2 = depth_fn (parse `((^(Pickled)) ^(Peppers) (^(Peppers) ^(Pickled)))`)
				val 5 = depth_fn (parse `((^(Pickled)) ((((^(Peppers))))) (^(Peppers) ^(Pickled)))`)
			in () end
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

	    ("test_pick", test_pick),
		("test_member", test_member),
		("test_union", test_union),
		("test_intersect", test_intersect),
		("test_intersect_all", test_intersect_all),
		("test_rember_upto_last", test_rember_upto_last),
		("test_leftmost", test_leftmost),
		("test_rember_one_star", test_rember_one_star),
		("test_rember_depth_star", test_depth_star)


	  ]

end
    
    
    
