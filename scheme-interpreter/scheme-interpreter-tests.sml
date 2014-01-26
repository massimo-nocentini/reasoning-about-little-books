structure SchemeInterpreterTests =
struct

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  structure SexpStr = MakeSexp ()

  structure SexpParser = SExpParserSMLofNJ (
      structure aSexp = SexpStr)

  structure SIE = SchemeInterpreterEnvironment (
      structure Sexp = SexpStr)

  structure Interpreter = SIE.MakeInterpreter ()

  structure SexpFunctions = SexpFunctionsStandardImpl (
      structure Sexp = SexpStr)

  open SIE Interpreter SexpStr;

  val assertEqualMeaning  = 
      let
	  fun equal_meaning (Quotation a, Quotation b) = 
	      SexpFunctions.equal SIE.scheme_term_equal a b
	    | equal_meaning (_, _) = false				  
      in
	  Assert.assertEqual equal_meaning
			     SIE.meaning_to_string
      end

  fun quoting_the_empty_list_should_return_the_empty_list () =
      let
  	  val sexp = SexpParser.parse `(^(TmQuote) ())`;
  	  val meaning = Interpreter.value sexp;
	  val expected = Quotation (List Null)

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning
      in
	  Assert.assertEqualString "(quote ())" input_as_string;
	  Assert.assertEqualString "()" output_as_string;
  	  assertEqualMeaning expected meaning
      end

  fun quoting_an_integer_should_return_that_integer () =
      let
  	  val sexp = SexpParser.parse `(^(TmQuote) ^(TmInteger 5))`;
  	  val meaning = Interpreter.value sexp;
	  val expected = Quotation (Atom (TmInteger 5))

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning

      in	  
	  Assert.assertEqualString "(quote 5)" input_as_string;
	  Assert.assertEqualString "5" output_as_string;
  	  assertEqualMeaning expected meaning
      end

  fun quoting_a_flat_list_should_return_that_list () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmQuote) (^(TmInteger 5) 
				       ^(TmBoolean false) 
				       ^(TmInteger 10)))`;
  	  val meaning = Interpreter.value sexp;
	  val expected = Quotation (List (Cons (
					 Atom (TmInteger 5),
					 Cons (
					     Atom (TmBoolean false),
					     Cons (
		 				 Atom (TmInteger 10),
						 Null)))))

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning

      in	  
	  Assert.assertEqualString "(quote (5 false 10))" input_as_string;
	  Assert.assertEqualString "(5 false 10)" output_as_string;
  	  assertEqualMeaning expected meaning
      end

  fun quoting_a_tree_with_unbounded_identifier_should_return_that_tree () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmQuote) (^(TmInteger 5) 
				       (
					 (
					   (^(TmIdentifier "x") 
					    ^(TmBoolean true))))
				       ^(TmInteger 10)))`;
  	  val meaning = Interpreter.value sexp;
	  val expected = Quotation (
		  List (
		      Cons (
			  Atom (TmInteger 5),
			  Cons (
			      List (
				  Cons (
				      List (
					  Cons (
					      List (
						  Cons (
						      Atom (TmIdentifier "x"),
							   Cons (
							   Atom (TmBoolean true), 
							   Null)
						      )),
					      Null)),
				      Null)),
			      Cons (
				  Atom (TmInteger 10),
				  Null)))))

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning

      in	  
	  Assert.assertEqualString "(quote (5 (((x true))) 10))" 
				   input_as_string;
	  Assert.assertEqualString "(5 (((x true))) 10)" output_as_string;
  	  assertEqualMeaning expected meaning
      end

  fun quoting_an_identifier_should_return_that_identifier_as_symbol () =
      let
  	  val sexp = SexpParser.parse `(^(TmQuote) ^(TmIdentifier "a"))`;
  	  val meaning = Interpreter.value sexp;
	  val expected = Quotation (Atom (TmIdentifier "a"))

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning
      in
	  Assert.assertEqualString "(quote a)" input_as_string;
	  Assert.assertEqualString "a" output_as_string;
  	  assertEqualMeaning expected meaning
      end

  fun consing_an_identifier_onto_an_empty_list_should_return_a_list_containing_only_that_identifier () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmCons) (^(TmQuote) ^(TmIdentifier "a"))
			             (^(TmQuote) ()))`;
  	  val meaning = Interpreter.value sexp;
	  val expected = Quotation (List (Cons (
					      Atom (TmIdentifier "a"),
					      Null)))

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning
      in
	  Assert.assertEqualString "(cons (quote a) (quote ()))" 
				   input_as_string;
	  Assert.assertEqualString "(a)" output_as_string;
  	  assertEqualMeaning expected meaning
      end

  fun consing_an_identifier_onto_a_tree_should_return_a_new_tree_with_that_identifier_in_very_left_child () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmCons) (^(TmQuote) ^(TmIdentifier "a"))
			             (^(TmQuote) (^(TmInteger 4) 
						       ((^(TmInteger 5)) 
							^(TmIdentifier "hello")))))`
  	  val meaning = Interpreter.value sexp

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning
      in
	  Assert.assertEqualString 
	      "(cons (quote a) (quote (4 ((5) hello))))" 
	      input_as_string;
	  Assert.assertEqualString "(a 4 ((5) hello))" output_as_string
      end

  fun consing_an_identifier_onto_a_non_list_should_raise_the_law_of_cons () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmCons)  (^(TmQuote) ^(TmIdentifier "a"))
				      (^(TmQuote) ^(TmIdentifier "b")))`;

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
      in
	  Assert.assertEqualString "(cons (quote a) (quote b))" 
				   input_as_string;
	  (Interpreter.value sexp; 
	   Assert.fail "Consing an atom onto another atom should raise the Law of Cons")
	   handle Law_of_Cons => ()
      end

  fun the_car_of_non_empty_list_with_an_int_at_heah_should_return_that_int () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmCar) (^(TmQuote) 
					  (^(TmInteger 4) 
						((^(TmInteger 5)) 
						 ^(TmIdentifier "hello")))))`	  
	  val expected = Quotation (Atom (TmInteger 4))
  	  val meaning = Interpreter.value sexp

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning
      in
	  Assert.assertEqualString 
	      "(car (quote (4 ((5) hello))))" 
	      input_as_string;
	  Assert.assertEqualString "4" output_as_string;
	  assertEqualMeaning expected meaning
      end

  fun car_of_empty_list_should_raise_the_law_of_car () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmCar)  (^(TmQuote) ()))`;

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
      in
	  Assert.assertEqualString "(car (quote ()))" 
				   input_as_string;
	  (Interpreter.value sexp; 
	   Assert.fail "Car of empty list should raise the Law of Car")
	   handle Law_of_Car => ()
      end

  fun cdr_of_non_empty_list_should_return_the_rest_of_that_list () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmCdr) (^(TmQuote) 
					  (^(TmInteger 4) 
						((^(TmInteger 5)) 
						 ^(TmIdentifier "hello")))))`	  
  	  val meaning = Interpreter.value sexp

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
	  val output_as_string = SIE.meaning_to_string meaning
      in
	  Assert.assertEqualString 
	      "(cdr (quote (4 ((5) hello))))" 
	      input_as_string;
	  Assert.assertEqualString "(((5) hello))" output_as_string
      end

  fun cdr_of_empty_list_should_raise_the_law_of_cdr () =
      let
  	  val sexp = SexpParser.parse 
			 `(^(TmCdr)  (^(TmQuote) ()))`;

	  val input_as_string = SexpFunctions.to_string 
				    SIE.term_to_string sexp
      in
	  Assert.assertEqualString "(cdr (quote ()))" 
				   input_as_string;
	  (Interpreter.value sexp; 
	   Assert.fail "Cdr of empty list should raise the Law of Cdr")
	   handle Law_of_Cdr => ()
      end


  fun suite () =
      Test.labelTests
      [
	("quoting_the_empty_list_should_return_the_empty_list", 
	 quoting_the_empty_list_should_return_the_empty_list),

	("quoting_an_integer_should_return_that_integer",
	 quoting_an_integer_should_return_that_integer ),

	("quoting_a_flat_list_should_return_that_list",
	 quoting_a_flat_list_should_return_that_list),

	("quoting_a_tree_with_unbounded_identifier_should_return_that_tree",
	 quoting_a_tree_with_unbounded_identifier_should_return_that_tree),

	("quoting_an_identifier_should_return_that_identifier_as_symbol",
	 quoting_an_identifier_should_return_that_identifier_as_symbol),

	("consing_an_identifier_onto_an_empty_list_should_return_a_list_containing_only_that_identifier",
	 consing_an_identifier_onto_an_empty_list_should_return_a_list_containing_only_that_identifier),

	("consing_an_identifier_onto_a_tree_should_return_a_new_tree_with_that_identifier_in_very_left_child",
	 consing_an_identifier_onto_a_tree_should_return_a_new_tree_with_that_identifier_in_very_left_child),

	("consing_an_identifier_onto_a_non_list_should_raise_the_law_of_cons", 
	 consing_an_identifier_onto_a_non_list_should_raise_the_law_of_cons),

	("the_car_of_non_empty_list_with_an_int_at_heah_should_return_that_int", 
	the_car_of_non_empty_list_with_an_int_at_heah_should_return_that_int),

	("car_of_empty_list_should_raise_the_law_of_car",
	 car_of_empty_list_should_raise_the_law_of_car),

	("cdr_of_non_empty_list_should_return_the_rest_of_that_list",
	 cdr_of_non_empty_list_should_return_the_rest_of_that_list),

	("cdr_of_empty_list_should_raise_the_law_of_cdr",
	 cdr_of_empty_list_should_raise_the_law_of_cdr)
      ]

end
    
