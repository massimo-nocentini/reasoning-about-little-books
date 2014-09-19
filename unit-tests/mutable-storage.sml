structure MutableStorageTests =
struct

	structure Assert = SMLUnit.Assert
	structure Test = SMLUnit.Test

	structure SexpStr = MakeSexp ()

	structure SexpParser = SExpParserSMLofNJ (
			structure Sexp = SexpStr) 

	val curried_equal = fn (fst: int ) => fn snd => fst = snd

	structure SexpEqualFunction = SexpEqual (structure Sexp = SexpStr)

	open SexpStr 
	open SexpParser

    structure SexpToStringFunction = SexpToString ( structure Sexp = SexpStr)

    fun assertPred pred item_to_string_fun = Assert.assertEqual 
        pred (SexpToStringFunction.to_string item_to_string_fun)

    fun assert_pred_on_integers pred = assertPred pred Int.toString

	val atom_eight = Atom 8
	val sexp_without_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(6) ^(7) ^(8))`
	val sexp_with_an_atom_in_two_in_a_row = parse `(^(1) ^(2) ^(3) ^(4) ^(5) ^(5) ^(7) ^(8))`
	val sexp_with_a_sexp_in_two_in_a_row = parse `((^(4)) (^(3) (^(2))) (^(3) (^(2))) ^(9))`

	fun eight_filter 8 = true
	|	eight_filter _ = false

    local
        datatype ingredients = Chocolate | Fruit | Cheese | Carrot | Cake

        structure ConsingOntoFixElementRememberingArgumentsFunction = 
            ConsingOntoFixElementRememberingArguments(
                type t = ingredients
                val fixed_element = Cake)
    in 
        fun test_sweet_tooth_remember () = 
            let 
                open ConsingOntoFixElementRememberingArgumentsFunction
                
                val {   result = [Chocolate, Cake], 
                        arguments_seen_so_far = [Chocolate] } = cons Chocolate
                val {   result = [Fruit, Cake], 
                        arguments_seen_so_far = [Fruit, Chocolate] } = cons Fruit
                val {   result = [Cheese, Cake], 
                        arguments_seen_so_far = [Cheese, Fruit, Chocolate] } = cons Cheese
                val {   result = [Carrot, Cake],
                        arguments_seen_so_far = [Carrot, Cheese, Fruit, Chocolate] } = cons Carrot
                val {   result = [Chocolate, Cake], 
                        arguments_seen_so_far = [Chocolate, Carrot, Cheese, Fruit, Chocolate] } = cons Chocolate
            in () end
    end

    local
        datatype pizza = Pizza

        fun pizza_to_string Pizza = "pizza"

        structure SexpConsCtorCountingForDeepSimple = 
            SexpConsCtorCounting (
                structure Sexp = SexpStr)
        structure DeepSimpleFunction = DeepSimple (
            structure Sexp = SexpStr
            structure SexpConsCtor = SexpConsCtorCountingForDeepSimple)

        structure SexpConsCtorCountingForDeepRemember = 
            SexpConsCtorCounting (
                structure Sexp = SexpStr)
        structure DeepRememberFunction = DeepRemember (
            structure Sexp = SexpStr
            structure SexpConsCtor = SexpConsCtorCountingForDeepRemember)

        structure SexpConsCtorCountingForDeepMemo = 
            SexpConsCtorCounting (
                structure Sexp = SexpStr)
        structure DeepMemoFunction = DeepMemo (
            structure Sexp = SexpStr
            structure SexpConsCtor = SexpConsCtorCountingForDeepMemo)

        val pizza_sexp_to_string = SexpToStringFunction.to_string pizza_to_string

        val deep_simple = DeepSimpleFunction.deep Pizza
        val deep_remember = DeepRememberFunction.deep Pizza
        val deep_memo = DeepMemoFunction.deep Pizza

        structure CounterIteratorForDeepSimple = CounterIterator(
            structure UnderCounting = 
                struct
                    type 'a fuc = int -> 'a 
                    val get_counter = SexpConsCtorCountingForDeepSimple.get_counter
                end)

        structure CounterIteratorForDeepRemember = CounterIterator(
            structure UnderCounting = 
                struct
                    type 'a fuc = int -> 'a 
                    val get_counter = SexpConsCtorCountingForDeepRemember.get_counter
                end)

        structure CounterIteratorForDeepMemo = CounterIterator(
            structure UnderCounting = 
                struct
                    type 'a fuc = int -> 'a 
                    val get_counter = SexpConsCtorCountingForDeepMemo.get_counter
                end)
    in 
        fun test_deep_simple () = 
            let

                val {   result,
                        memo_table = [(3, value)]  } = deep_simple 3
                val "(((pizza)))" = pizza_sexp_to_string value
                val "(((pizza)))" = pizza_sexp_to_string result
                val 3 = SexpConsCtorCountingForDeepSimple.get_counter ()

                val {   result,
                        memo_table = [(5, five_sexp),(3, three_sexp)]  } = deep_simple 5
                val "(((pizza)))" = pizza_sexp_to_string three_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string five_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string result
                val 8 = SexpConsCtorCountingForDeepSimple.get_counter ()

                val {   result,
                        memo_table = [(3, three_sexp_doubled),(5, five_sexp),(3, three_sexp)]  } = deep_simple 3
                val "(((pizza)))" = pizza_sexp_to_string three_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string five_sexp
                val "(((pizza)))" = pizza_sexp_to_string three_sexp_doubled
                val "(((pizza)))" = pizza_sexp_to_string result
                val 11 = SexpConsCtorCountingForDeepSimple.get_counter ()

                val _ = SexpConsCtorCountingForDeepSimple.set_counter 0
                val 0 = SexpConsCtorCountingForDeepSimple.get_counter ()
                val 500500 = CounterIteratorForDeepSimple.super_counter deep_simple 1000  

            in () end 

        fun test_deep_remember () = 
            let
                val deep = DeepRememberFunction.deep Pizza

                val {   result,
                        memo_table = [(3, value)]  } = deep 3
                val "(((pizza)))" = pizza_sexp_to_string value
                val "(((pizza)))" = pizza_sexp_to_string result
                val 3 = SexpConsCtorCountingForDeepRemember.get_counter () 

                val {   result,
                        memo_table = [(5, five_sexp),(3, three_sexp)]  } = deep 5
                val "(((pizza)))" = pizza_sexp_to_string three_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string five_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string result
                val 8 = SexpConsCtorCountingForDeepRemember.get_counter ()

                val {   result,
                        memo_table = [(5, five_sexp),(3, three_sexp)]  } = deep 3
                val "(((pizza)))" = pizza_sexp_to_string result
                val 8 = SexpConsCtorCountingForDeepRemember.get_counter () 

                val {   result,
                        memo_table = [(6, six_sexp),(5, five_sexp),(3, three_sexp)]  } = deep 6
                val "((((((pizza))))))" = pizza_sexp_to_string six_sexp
                val "((((((pizza))))))" = pizza_sexp_to_string result
                val 14 = SexpConsCtorCountingForDeepRemember.get_counter () 

                val {   result,
                        memo_table = [(9, nine_sexp),(6, six_sexp),(5, five_sexp),(3, three_sexp)]  } = deep 9
                val "(((((((((pizza)))))))))" = pizza_sexp_to_string nine_sexp
                val "(((((((((pizza)))))))))" = pizza_sexp_to_string result
                val 23 = SexpConsCtorCountingForDeepRemember.get_counter ()

                val _ = SexpConsCtorCountingForDeepRemember.set_counter 0
                val 0 = SexpConsCtorCountingForDeepRemember.get_counter ()
                val 500500 = CounterIteratorForDeepRemember.super_counter deep_remember 1000  
            in () end

        fun test_deep_memo () = 
            let
                val deep = DeepMemoFunction.deep Pizza

                val {   result,
                        memo_table = [(3, three_sexp),(2, two_sexp),(1, one_sexp),(0, pizza_sexp)]  } = deep 3
                val "pizza" = pizza_sexp_to_string pizza_sexp
                val "(pizza)" = pizza_sexp_to_string one_sexp
                val "((pizza))" = pizza_sexp_to_string two_sexp
                val "(((pizza)))" = pizza_sexp_to_string three_sexp
                val "(((pizza)))" = pizza_sexp_to_string result 
                val 3 = SexpConsCtorCountingForDeepMemo.get_counter () 

                val {   result,
                        memo_table = [(5, five_sexp),(4, four_sexp),
                            (3, three_sexp),(2, two_sexp),(1, one_sexp),(0, pizza_sexp)]  } = deep 5
                val "pizza" = pizza_sexp_to_string pizza_sexp
                val "(pizza)" = pizza_sexp_to_string one_sexp
                val "((pizza))" = pizza_sexp_to_string two_sexp
                val "(((pizza)))" = pizza_sexp_to_string three_sexp
                val "((((pizza))))" = pizza_sexp_to_string four_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string five_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string result 
                val 5 = SexpConsCtorCountingForDeepMemo.get_counter () 

                val {   result,
                        memo_table = [(6, six_sexp), (5, five_sexp),(4, four_sexp),
                                 (3, three_sexp),(2, two_sexp), (1, one_sexp),(0, pizza_sexp)]  } = deep 6
                val "pizza" = pizza_sexp_to_string pizza_sexp
                val "(pizza)" = pizza_sexp_to_string one_sexp
                val "((pizza))" = pizza_sexp_to_string two_sexp
                val "(((pizza)))" = pizza_sexp_to_string three_sexp
                val "((((pizza))))" = pizza_sexp_to_string four_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string five_sexp
                val "((((((pizza))))))" = pizza_sexp_to_string six_sexp
                val "((((((pizza))))))" = pizza_sexp_to_string result 
                val 6 = SexpConsCtorCountingForDeepMemo.get_counter () 

                val {   result,
                        memo_table = [(9, nine_sexp),(8, eight_sexp),(7, seven_sexp),(6, six_sexp),
                            (5, five_sexp),(4, four_sexp), (3, three_sexp),(2, two_sexp),
                            (1, one_sexp),(0, pizza_sexp)]  } = deep 9
                val "pizza" = pizza_sexp_to_string pizza_sexp
                val "(pizza)" = pizza_sexp_to_string one_sexp
                val "((pizza))" = pizza_sexp_to_string two_sexp
                val "(((pizza)))" = pizza_sexp_to_string three_sexp
                val "((((pizza))))" = pizza_sexp_to_string four_sexp
                val "(((((pizza)))))" = pizza_sexp_to_string five_sexp
                val "((((((pizza))))))" = pizza_sexp_to_string six_sexp
                val "(((((((pizza)))))))" = pizza_sexp_to_string seven_sexp
                val "((((((((pizza))))))))" = pizza_sexp_to_string eight_sexp
                val "(((((((((pizza)))))))))" = pizza_sexp_to_string nine_sexp
                val "(((((((((pizza)))))))))" = pizza_sexp_to_string result 
                val 9 = SexpConsCtorCountingForDeepMemo.get_counter () 

                val _ = SexpConsCtorCountingForDeepMemo.set_counter 0
                val 0 = SexpConsCtorCountingForDeepMemo.get_counter ()
                val 1000 = CounterIteratorForDeepMemo.super_counter deep_memo 1000  
            in () end

    end

    local
        datatype pizza = Pizza | Mozzarella | Cake

        fun pizza_to_string Pizza = "pizza"
        |   pizza_to_string Mozzarella = "mozzarella"
        |   pizza_to_string Cake = "cake"

        val pizza_sexp_to_string = SexpToStringFunction.to_string pizza_to_string

        structure DeepToppingsWithLetccFunction = DeepToppingsWithLetcc (
            structure Sexp = SexpStr
            structure HopSkipAndJump = HopSkipAndJumpFunctor (
                structure Cont = SMLofNJ.Cont))

        structure DeepWithToppingsWithLetccFunction = DeepWithToppingsWithLetcc (
            structure Sexp = SexpStr
            structure HopSkipAndJump = HopSkipAndJumpFunctor (
                structure Cont = SMLofNJ.Cont))

        structure DeepToppingsWithContFunction = DeepToppingsWithCont (
            structure Sexp = SexpStr
            structure HopSkipAndJump = HopSkipAndJumpFunctor (
                structure Cont = SMLofNJ.Cont))

        structure DeepContWithLetccFunction = DeepContWithLetcc (
            structure Sexp = SexpStr
            structure HopSkipAndJump = HopSkipAndJumpFunctor (
                structure Cont = SMLofNJ.Cont))
    in 
        fun test_deep_toppings_with_letcc () =
            let 
                (*val deep_six_layer = DeepToppingsWithLetccFunction.deep Pizza 6*)
                (*
                 The following is another test in order to "catch" the continuation using a 
                 single ML expression, ie wrapping everything inside a pair of brackets ().
                 However it is not sufficient since the test will loop once more.
                 *)
                (*val test = ref NONE
                val "((((((mozzarella))))))" = pizza_sexp_to_string (
                    (test := SOME (DeepToppingsWithLetccFunction.deep Pizza 6);
                     (valOf (!test))  Mozzarella))*)

                val {result = six_layer_pizza_sexp, toppings = six_layer_toppings } = 
                    DeepWithToppingsWithLetccFunction.deep 6 Pizza
                val "((((((pizza))))))" = pizza_sexp_to_string six_layer_pizza_sexp
                (*val "pizza" = pizza_sexp_to_string (six_layer_toppings Pizza) 
                val "mozzarella" = pizza_sexp_to_string (six_layer_toppings Pizza) *)
                
                val six_layer = DeepToppingsWithContFunction.deep Pizza 6
                (*val "((((((pizza))))))" = pizza_sexp_to_string (six_layer Pizza) *)
                val "((((((pizza))))))" = pizza_sexp_to_string (six_layer Pizza) 
(*                val moz = pizza_sexp_to_string (six_layer Mozzarella)  *)
(*                val six_layer = DeepToppingsWithLetccFunction.deep Pizza 6
                val "((((((pizza))))))" = pizza_sexp_to_string (six_layer Pizza) 
                val "((((((mozzarella))))))" = pizza_sexp_to_string (deep_six_layer Mozzarella)
                val "((((((cake))))))" = pizza_sexp_to_string (deep_six_layer Cake) *)
 
                val six_layer_option_ref = ref NONE
                val _ = DeepContWithLetccFunction.deep Pizza 6 (
                    fn six_layer => 
                        let 
                            val _ = six_layer_option_ref := SOME six_layer
                            val "((((((pizza))))))" = pizza_sexp_to_string (six_layer Pizza)
                            val "((((((mozzarella))))))" = pizza_sexp_to_string (six_layer Mozzarella)
                            val "((((((cake))))))" = pizza_sexp_to_string (six_layer Cake)
                            val "((((((cake))))))" = pizza_sexp_to_string (List (Cons (six_layer Cake, Null)))
                            val "((((((mozzarella))))))" = pizza_sexp_to_string 
                                (List (Cons (List (Cons (List (Cons (six_layer Mozzarella, Null)), Null)), Null)))
                        in () end)
(*                val SOME six_layer = !six_layer_option_ref
                val "((((((mozzarella))))))" = pizza_sexp_to_string 
                    (List (Cons (List (Cons (List (Cons (six_layer Mozzarella, Null)), Null)), Null))) *)
            in () end 
    end

    local 

        structure SexpBizarreForImperativeYFunction = 
            SexpBizarreForImperativeY (
                structure ImperativeY = Y_imperative_one_arg ())

        structure SexpBizarreForApplicativeYFunction = 
            SexpBizarreForApplicativeY(
                structure ApplicativeY = Y_applicative_one_arg_from_little_lisper (
                    structure SelfApplication = SelfApplication()))

        structure AnotherSexpBizarreForApplicativeYFunction = 
            SexpBizarreForApplicativeY(
                structure ApplicativeY = Y_applicative_one_arg_from_little_lisper (
                    structure SelfApplication = SelfApplication()))
    in

        (*
         For this test we do not use our pattern for testing multiple implementation
         of the same function since for some test cases the implementation via
         Y imperative doesn't halt at all!. Soon we provide test cases for the implementation
         via Y applicative, which should always halt.
         *)
        fun test_bizarre_for_y_imperative () =
            let 
                val bizarre_via_y_imperative =  SexpBizarreForImperativeYFunction.bizarre
                val bizarre_via_y_applicative =  SexpBizarreForApplicativeYFunction.bizarre

                val 0 = bizarre_via_y_imperative 1
                (* The two following test do not halt :) 
                val 0 = bizarre_via_y_imperative 5
                *)

                val 0 = AnotherSexpBizarreForApplicativeYFunction.bizarre 1
                val 0 = AnotherSexpBizarreForApplicativeYFunction.bizarre 5
                (* 
                 The following invocation doesn't work because, since the Y 
                 applicative does increment the reference cell defined in `bizarre',
                 moving it to 5 will evaluate all the following equality checks with 2 
                 to false, incrementing the counter again and again. Hence
                 we've to define another structure, such that a new reference cell
                 is created, in particular, a new environment for `bizarre' is created.
                 *)
                (* val 0 = AnotherSexpBizarreForApplicativeYFunction.bizarre 2 *)
                val 0 = SexpBizarreForApplicativeYFunction.bizarre 1

                (* Finally observe that it does not make sense to invoke `bizarre 0' :)*)
            in () end

    end

	fun suite () = Test.labelTests [
        ("test_sweet_tooth_remember", test_sweet_tooth_remember),
        ("test_deep_simple", test_deep_simple),
        ("test_deep_remember", test_deep_remember),
        ("test_deep_memo", test_deep_memo),
        ("test_bizarre_for_y_imperative", test_bizarre_for_y_imperative),
        ("test_deep_toppings_with_letcc", test_deep_toppings_with_letcc)
	]

	end

