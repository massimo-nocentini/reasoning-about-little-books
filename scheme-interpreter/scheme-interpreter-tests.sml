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

  fun assertEqualMeaning pred item_to_string_fun = 
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
      in
  	  assertEqualMeaning expected meaning
      end
  
  fun suite () =
      Test.labelTests
      [
        (* ("length of complex shuffled tree using length defined with Y combinator using collector technique", *)
	(*  length_of_complex_shuffled_tree (LittleSchemer.length_with_collector (fn length => length))) *)


      ]

end
    
