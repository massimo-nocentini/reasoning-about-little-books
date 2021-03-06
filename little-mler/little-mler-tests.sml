structure LittleMLerTests =
struct

  
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  structure SexpStr = MakeSexp ()
  structure LittleMLerStr = LittleMLer (structure SexpStr = SexpStr)
  structure SexpToStringFunction = SexpToString (structure Sexp = SexpStr)

  open SexpStr 
  open LittleMLerStr 

  fun assertPred pred item_to_string_fun = 
      Assert.assertEqual pred (SexpToStringFunction.to_string item_to_string_fun)

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

  local 
      structure NS = NumberAsNumWithRevealConceal ()
      structure NA = PlusOverNumberWithWhereClause (
	  structure a_N = NS)
  in
  fun first_attempt_of_num_plus_using_functors_with_where_clause_in_signature_result () =
      let	  
	  open NS
	  open NA

	  val computed = reveal (plus (conceal 9) (conceal 3))
      in
	  Assert.assertEqualInt 12 computed
      end
  end

  local 
      structure IS = NumberAsIntWithRevealConceal ()
      structure IA = PlusOverNumberWithWhereClause (
	  structure a_N = IS)
  in
  fun first_attempt_of_int_plus_using_functors_with_where_clause_in_signature_result () =
      let	  
	  open IS
	  open IA

	  val computed = reveal (plus (conceal 9) (conceal 3))
      in
	  Assert.assertEqualInt 12 computed
      end
  end

  fun second_attempt_of_int_plus_using_functors_with_where_clause_in_signature_result () =
      let 
	  open IntArith'
	  val computed = plus 1 2
      in 
	  Assert.assertEqualInt 3 computed
      end

  fun similarity_among_0_and_Zero_should_be_satisfied () = 
      let
	  open SimIntNum
	  val computed = similar (IntStructWithRevealConceal.conceal 0)
				 (NumStructWithRevealConceal.conceal 0)
      in
	  Assert.assertEqualBool true computed
      end

  fun similarity_among_1_and_Zero_should_not_be_satisfied () = 
      let
	  open SimIntNum
	  val computed = similar (IntStructWithRevealConceal.conceal 1)
				 (NumStructWithRevealConceal.conceal 0)
      in
	  Assert.assertEqualBool false computed
      end

  fun plus_using_NewPlusOverJ () = 
      let
	  open NewPlusStruct
	  val computed = plus 3 4
      in
	  Assert.assertEqualInt 7 computed
      end

  fun times_using_TimesOverNumbers () = 
      let
	  open TimesStruct
	  val computed = NumStructWithRevealConceal.reveal 
			     (times (NumStructWithRevealConceal.conceal 8)
				    (NumStructWithRevealConceal.conceal 7))
      in
	  Assert.assertEqualInt 56 computed
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
	("reveal conseal of NumStruct", reveal_conseal_of_NumStruct),
	("first attempt of plus among ``int''susing functors with where clause in signature result",
	 first_attempt_of_num_plus_using_functors_with_where_clause_in_signature_result),
	("first attempt of plus among ``num''s using functors with where clause in signature result",
	 first_attempt_of_int_plus_using_functors_with_where_clause_in_signature_result),
	("second attempt of plus among ``num''s using functors with where clause in signature result",
	 second_attempt_of_int_plus_using_functors_with_where_clause_in_signature_result),
	("similarity among 0 and Zero should be satisfied",
	similarity_among_0_and_Zero_should_be_satisfied),
	("similarity among 1 and Zero should not be satisfied",
	similarity_among_1_and_Zero_should_not_be_satisfied),
	("plus using NewPlusOverJ", plus_using_NewPlusOverJ),
	("times using TimesOverNumbers", times_using_TimesOverNumbers)
      ]

end
    
    
