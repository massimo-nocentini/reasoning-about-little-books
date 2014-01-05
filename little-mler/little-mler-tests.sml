structure LittleMLerTests =
struct

  open LittleMLer
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun assertPred pred item_to_string_fun = 
      Assert.assertEqual pred (SExpressions.to_string item_to_string_fun)

  fun assert_pred_on_integers pred = assertPred pred Int.toString

  fun check_123_prefixing prefixer () = 
      let 
	  val computed = prefixer (parse `((^(5)) ((^(9) ^(3))) ^(0))`)
	  val expected = parse `(^(1) ^(2) ^(3) (^(5)) ((^(9) ^(3))) ^(0))`
      in 
	  assert_pred_on_integers (op =) expected computed 
      end

  fun reveal_conseal_of_IntStruct () = 
      let 
	  open IntStructWithRevealConceal
	  val computed = reveal (succ (conceal 0))
      in
	  Assert.assertEqualInt 1 computed
      end

  (* here it should be useful to have the possibility to handle
structure as first class citizen: the testing code is equal to the
testing code of reveal_conseal_of_IntStruct *)
  fun reveal_conseal_of_NumStruct () = 
      let 
	  open NumStructWithRevealConceal
	  val computed = reveal (succ (conceal 0))
      in
	  Assert.assertEqualInt 1 computed
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
	 check_123_prefixing prefixer),
	("reveal conseal of IntStruct", reveal_conseal_of_IntStruct),
	("reveal conseal of NumStruct", reveal_conseal_of_NumStruct)
      ]

end
    
    
