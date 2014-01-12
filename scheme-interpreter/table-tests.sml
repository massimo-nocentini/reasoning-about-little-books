structure TableTests =
struct


  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  datatype thing = Ball | Lamp | Laptop 
		   | ForKeyNotFound | ForValuesNotEnough

  functor MakeTypeThing () 
	:> TYPE where type aType = thing
        =
	struct type aType = thing end

  structure Table = MakeTableDoubleListImpl (
      structure IdentifierType = MakeTypeString ()
      structure StuffType = MakeTypeThing ());

  open Table

  val key_one = "key_one"
  val key_two = "key_two"
  val key_three = "key_three"

  val (keys_one_to_one, values_one_to_one) = 
      ([key_one, key_two], [Ball, Laptop])

  val (keys_missing_key, values_missing_key) = 
      ([key_one, key_two], [Lamp, Laptop, Laptop])

  val (keys_values_not_enough, values_values_not_enough) = 
      ([key_one, key_two, key_three], [Laptop, Laptop])

  val one_to_one_entry = new_entry keys_one_to_one values_one_to_one
  val missing_key_entry = new_entry keys_missing_key values_missing_key
  val values_not_enough_entry = new_entry keys_values_not_enough
					  values_values_not_enough

  fun build_entry_with_exact_keys_values_matching_should_succeed () = 
      let
	  val computed = new_entry keys_one_to_one values_one_to_one
      in
	  Assert.assertTrue true
      end

  fun build_entry_with_more_values_than_keys_should_succeed () = 
      let
	  val computed = new_entry keys_missing_key values_missing_key
      in
	  Assert.assertTrue true
      end

  fun build_entry_with_not_enough_values_matching_should_succeed () = 
      let
	  val computed = new_entry keys_values_not_enough 
				   values_values_not_enough
      in
	  Assert.assertTrue true
      end

  fun looking_for_a_key_present_in_an_entry_should_succeed () =
      let
  	  exception ShouldNotBeCalled
  	  val table = extend_table one_to_one_entry empty_table
  	  fun pred key = key = key_two
  	  fun failure_handler _ = raise ShouldNotBeCalled
  	  val value = lookup_in_table pred table failure_handler
      in
  	  Assert.assert "should be Laptop" (value = Laptop)
	  handle ShouldNotBeCalled => 
		 Assert.fail "the failure handler shouldn't be called."
      end

  fun looking_for_a_key_present_in_an_entry_should_succeed () =
      let
  	  exception ShouldNotBeCalled
  	  val table = extend_table one_to_one_entry empty_table

  	  fun pred key = key = key_three (* the missing one *)

  	  fun failure_handler (KeyNotFound _) = ForKeyNotFound
	    | failure_handler _ = raise ShouldNotBeCalled

  	  val value = lookup_in_table pred table failure_handler
      in
  	  Assert.assert "should be some stuff for not found key." 
			(value = ForKeyNotFound)
	  handle ShouldNotBeCalled => 
		 Assert.fail "the failure handler should be called only when a key wasn't found."
      end


  fun looking_for_a_key_present_in_two_entries_should_return_the_value_in_the_former_entry () =
      let
  	  exception ShouldNotBeCalled
	  val temp_table = extend_table one_to_one_entry empty_table
  	  val table = extend_table missing_key_entry temp_table
  	  fun pred key = key = key_one
  	  fun failure_handler _ = raise ShouldNotBeCalled
  	  val value = lookup_in_table pred table failure_handler
      in
  	  Assert.assert "should be Lamp instead of Ball" (value = Lamp)
	  handle ShouldNotBeCalled => 
		 Assert.fail "the failure handler shouldn't be called."
      end

  fun looking_for_a_key_present_in_an_entry_where_values_arent_enough () =
      let
  	  exception ShouldNotBeCalled
  	  val table = extend_table values_not_enough_entry empty_table

  	  fun pred key = key = key_three (* the unmatched one *)

  	  fun failure_handler (ValuesNotEnough _) = ForValuesNotEnough
	    | failure_handler _ = raise ShouldNotBeCalled

  	  val value = lookup_in_table pred table failure_handler
      in
  	  Assert.assert "should be some stuff for not enough values." 
			(value = ForValuesNotEnough)
	  handle ShouldNotBeCalled => 
		 Assert.fail "the failure handler should be called only when values aren't enough."
      end


  fun suite () =
      Test.labelTests
      [
	("build_entry_with_exact_keys_values_matching_should_succeed",
	 build_entry_with_exact_keys_values_matching_should_succeed),
	("build_entry_with_more_values_than_keys_should_succeed",
	 build_entry_with_more_values_than_keys_should_succeed),
	("build_entry_with_not_enough_values_matching_should_succeed",
	 build_entry_with_not_enough_values_matching_should_succeed),
	("looking_for_a_key_present_in_an_entry_should_succeed",
	 looking_for_a_key_present_in_an_entry_should_succeed),
	("looking_for_a_key_present_in_an_entry_should_succeed",
	 looking_for_a_key_present_in_an_entry_should_succeed),
	("looking_for_a_key_present_in_two_entries_should_return_the_value_in_the_former_entry", 
	 looking_for_a_key_present_in_two_entries_should_return_the_value_in_the_former_entry),
	("looking_for_a_key_present_in_an_entry_where_values_arent_enough",
	 looking_for_a_key_present_in_an_entry_where_values_arent_enough)
      ]

end
    
